package tryp
package app

import scalaz.{Tree => STree, _}
import Scalaz._

import monocle.macros._
import monocle.std._
import monocle.syntax._
import monocle.function._
import monocle.Lens

trait ResourceMetadata
extends AttrMetadata
{
  import c.universe._

  type ModelTreeLoc = TreeLoc[ModelTreeNode]

  abstract class ResourceSpec
  {
    def tpe: TypeName
    def params: Attrs
    def bases: List[Tree]
    def basic: ModelTreeLoc

    def name = tpe.toString

    def term = tpe.toTermName

    def fqTerm(tail: TermName) = {
      (IList(basic.parentTerms: _*) <::: NonEmptyList(tail)).select[TermName]
    }

    def fieldType[A <: AttrSpecBase: ClassTag]: List[A] = params.collect {
      case f: A => f
    }

    def notFieldType[A <: AttrSpecBase: ClassTag] = params.filter {
      case _: A => false
      case _ => true
    }

    def refSingles = fieldType[RefSingleSpec]

    def refLists = fieldType[RefListSpec]

    def embeddedSingle = fieldType[EmbeddedSingleSpec]

    def embeddedLists = fieldType[EmbeddedListSpec]

    def refParams = fieldType[RefSpec]

    def embeddedParams = fieldType[EmbeddedSpec]

    def flatParams = notFieldType[EmbeddedSpec]

    def modelPath = basic.modelPath
  }

  trait ModelSpecBase
  extends ResourceSpec
  with HasClass
  {
    def body: BodyData
    def tpe = cls.tpe
    def modules = body.modules
    def excludes = cls.excludes
    def ident = cls.ident
    def compactString = s"$tpe(${params map(_.term) mkString(",")})"
  }

  @Lenses
  case class ModelSpec(
    cls: ClassBase, params: Attrs, bases: List[Tree],
    body: BodyData, basic: ModelTreeLoc, origin: Option[ModelTreeLoc],
    path: Option[String]
  )
  extends ModelSpecBase

  object ModelSpec
  {
    object Zero
    extends ModelSpec(
      ClassBase.ClassBaseMonoid.zero, Nil, Nil, BodyData.BodyDataMonoid.zero,
      ModelTreeNode.zero.loc, None, Some("invalid"))

    val zeroTree = (Zero: ModelSpec).leaf

    implicit object ShowModelSpec
    extends Show[ModelSpec]
    {
      override def show(spec: ModelSpec) = spec.compactString
    }

    implicit object `ModelSpec Liftable`
    extends Liftable[ModelSpec]
    {
      def apply(m: ModelSpec) = {
        q"""
        case class ${m.tpe}(..${m.params})
        extends ..${m.bases}
        {
          ..${m.body}
        }
        """
      }
    }

    implicit val seqLifter = new SeqLifter[ModelSpec] {
      def liftSeq(m: ModelSpec) = List(q"$m")
    }

    implicit object `ModelSpec EmbedLifter`
    extends BodyEmbedLifter[ModelSpec]
    {
      def embedLens = GenLens[ModelSpec](_.body)
    }

    implicit object ModelSpecMonoid
    extends Monoid[ModelSpec]
    {
      val zero = ModelSpec.Zero

      def append(a: ModelSpec, b: => ModelSpec) = {
        (a, b) match {
          case (ModelSpec.Zero, m) => m
          case (m, ModelSpec.Zero) => m
          case _ =>
            val node = a.basic.hasParents ? a.basic | b.basic
            val origin = a.basic.hasParents ? a.origin | b.origin
            ModelSpec(
              a.cls ⊹ b.cls,
              a.params ⊹ b.params,
              a.bases ⊹ b.bases,
              a.body ⊹ b.body,
              node,
              origin,
              a.path orElse b.path
            )
        }
      }
    }

    implicit def ModelSpecListMonoid = new Monoid[List[ModelSpec]] {
      def zero = List()

      def append(a: List[ModelSpec], b: => List[ModelSpec]) = {
        orderPreservingMerge(a, b)(_.ident)
      }
    }

    implicit object ModelSpecTreeMonoid
    extends Monoid[STree[ModelSpec]]
    {
      def zero = (ModelSpec.Zero: ModelSpec).node()

      def append(a: STree[ModelSpec], b: => STree[ModelSpec]) = {
        val sub = ModelSpecTreeSeqMonoid.append(a.subForest, b.subForest)
        (a.rootLabel ⊹ b.rootLabel).node(sub: _*)
      }
    }

    implicit object ModelSpecTreeSeqMonoid
    extends Monoid[Seq[STree[ModelSpec]]]
    {
      def zero = Seq()

      def append(a: Seq[STree[ModelSpec]], b: => Seq[STree[ModelSpec]]) = {
        orderPreservingMerge(a, b)(_.ident)
      }
    }
  }

  implicit object `EnumSpec Liftable`
  extends Liftable[EnumSpec]
  {
    def apply(m: EnumSpec) = {
      q"""
      object ${m.term}
      extends Enumeration
      {
        ..${m.body}
      }
      """
    }
  }

  type ModelTree = STree[ModelSpec]

  object ModelTree
  {
    def root: ModelSpec = ModelSpec.Zero
    def apply(sub: Seq[ModelTree]) = root.node(sub: _*)
  }

  implicit object ModelTreeShow
  extends Show[ModelTree]
  {
    override def show(t: ModelTree) = s"ModelTree: ${t.rootLabel.show}"
  }

  implicit class ModelTreeOps(tree: ModelTree)
  {
    def subByType(tpe: Name) = {
      tree.subForest.find(_.rootLabel.tpe.toString == tpe.toString)
    }

    def basicModelTree = {
      tree flatMap(m => ModelTreeNode.model(m.cls).leaf)
    }

    def ident = tree.rootLabel.ident

    def cls = tree.rootLabel.cls

    def mobile = {
      tree
        .subForest
        .labels
        .filter(_.cls.mobile)
        .flatMap(_.embeddedParams)
    }
  }

  trait Embed[A]

  trait SeqLifter[A]
  {
    def liftSeq(a: A): Seq[Tree]
  }

  abstract class EmbedLifter[A]
  {
    def liftSub(a: A, sub: Seq[Tree]): Seq[Tree]
  }

  abstract class BodyEmbedLifter[A](implicit sl: SeqLifter[A])
  extends EmbedLifter[A]
  {
    def embedLens: Lens[A, BodyData]

    def liftSub(a: A, sub: Seq[Tree]) = liftIntoModule(a, sub)

    def liftIntoModule(a: A, sub: Seq[Tree]) = {
      sl.liftSeq {
        embedLens.append(BodyData.parse(sub.toList))(a)
      }
    }
  }

  implicit object `ModuleDef EmbedLifter`
  extends EmbedLifter[ModuleDef]
  {
    def liftSub(m: ModuleDef, sub: Seq[Tree]) = {
      val i = m.impl
      ModuleDef(m.mods, m.name, Template(i.parents, i.self, i.body ++ sub)) ::
        Nil
    }
  }

  implicit object `ClassDef EmbedLifter`
  extends EmbedLifter[ClassDef]
  {
    def liftSub(m: ClassDef, sub: Seq[Tree]) = {
      val i = m.impl
      ClassDef(m.mods, m.name, m.tparams,
        Template(i.parents, i.self, i.body ++ sub)) ::
        Nil
    }
  }

  case class WrappedModule(mod: ModuleDef, rest: List[Tree])

  object WrappedModule
  {
    implicit val embedLifter = new EmbedLifter[WrappedModule]
    {
      def liftSub(m: WrappedModule, sub: Seq[Tree]) = {
        `ModuleDef EmbedLifter`.liftSub(m.mod, sub) ::: m.rest
      }
    }
  }

  @Lenses
  case class ModelsSpec(
    models: ModelTree,
    enums: List[EnumSpec],
    body: BodyData
  )
  extends ModelsSpecBase
  {
    lazy val modelMap = (models foldRight Map[String, ModelSpec]()) {
      (spec, map) =>
        map + (spec.name -> spec)
    }

    def modelsFor(targets: String*) = {
      val result = models.subForest
        .filter(_.rootLabel.excludes.intersect(targets).isEmpty)
      (implicitly[Monoid[ModelSpec]].zero: ModelSpec).node(result: _*)
    }

    def modelTreeForEmbed(s: EmbeddedSpec) = {
      s.actualType.segments.foldLeft(models) { case (current, tpe) =>
        current.subByType(tpe) getOrAbort(
          s"Could not find '${s.actualType}' in model tree:" +
          s"\n${models.drawTree}\n\nat '$tpe' in \n${current.drawTree}"
        )
      }
    }

    def rerootModels(top: Seq[ModelTree], basic: BasicModelTree) = {
      top map { tree =>
        tree map { node =>
          val newLoc = basic
            .modelLoc(node)
            .getOrAbort(s"Could not find node for relocation of $node")
          modelsParser.relocate(node, basic, newLoc)
        }
      }
    }

    def extractModels(params: Seq[EmbeddedSpec]): List[ModelTree] = {
      params.toList
        .map(modelTreeForEmbed)
        .flatMap(a => a :: extractModels(a.rootLabel.embeddedParams))
        .distinct
    }

    def isTopLevel(spec: ModelTree) = {
      models.subForest map(_.rootLabel.cls) contains(spec.rootLabel.cls)
    }

    def topLevel = models.topLevel

    /* For mobile, only a user's own models are duplicated onto the device db.
     * To realize that, all models marked by '@mobile' are treated as
     * containers for user data.
     * Only embedded fields of those containers and their dependencies are used
     * for the slick schema.
     * For that purpose, a new basic model tree must be established, containing
     * the embedded fields as top level models, plus the models depended on
     * that they were already at top level.
     * The models corresponding to the embedded fields are then tranformed to
     * point to the new model tree, while keeping the old model in an extra
     * param.
     * An additional 'path' argument is specified that contains the embedded
     * attribute's name.
     */
    def mobileModels = {
      val embedded = models.mobile
      val mobile = embedded map { par =>
        modelTreeForEmbed(par) map(ModelSpec.path.set(Some(par.name)))
      }
      val existing = mobile.labels map(_.cls)
      val missing = extractModels(embedded)
        .filter(isTopLevel)
      val forced = models.subForest filter(_.rootLabel.cls.forceMobile)
      val regular = (missing ++ forced)
        .filterNot(a => existing.contains(a.cls))
      val basic = ModelTreeNode((regular ++ mobile) map(_.basicModelTree))
      ModelTree(regular ++ rerootModels(mobile, basic))
    }

    def mergeTree(tree: Tree) = {
      Some(tree) collect(unliftStruct) match {
        case Some(s @ ClassData(_, _, _, _)) => mergeClass(s)
        case Some(s @ ModuleData(_, _, _)) => mergeModule(s)
        case None => addMisc(tree)
        case _ =>
          c.abort(c.enclosingPosition,
            "Tried to merge invalid struct into ModelsSpec")
      }
    }

    def mergeTrees(trees: Tree*) = {
      trees.foldLeft(this) { (spec, tree) => spec.mergeTree(tree) }
    }

    def mergeStructs[A <: StructData: Monoid](structs: List[A],
      lens: monocle.Lens[BodyData, List[A]]) = {
        Log.d(s"ModelsSpec: merging structs: $structs; existing: $body")
      (ModelsSpec.body ^|-> lens)
        .modify(_ ⊹ structs)(this)
    }

    def mergeModule(m: ModuleBase) = mergeModules(List(m))
    def mergeClass(c: ClassBase) = mergeClasses(List(c))
    def mergeModules(m: List[ModuleBase]) = mergeStructs(m, BodyData.modules)
    def mergeClasses(c: List[ClassBase]) = mergeStructs(c, BodyData.classes)
    def mergeBody(b: BodyData) = {
      mergeModules(b.modules).mergeClasses(b.classes)
    }

    def addMisc(tree: Tree) =
      (ModelsSpec.body ^|-> BodyData.misc).modify(_ :+ tree)(this)

    def modelForAttr(a: AttrSpecBase) = {
      modelMap.get(a.actualType.toString).getOrElse(
        c.abort(c.enclosingPosition,
          s"No model class found for embedded attr $a"
          )
      )
    }

    lazy val modelNames = models.toList map(_.cls.ident)

    override def toString = {
      val modelDescs = models.toList map(_.compactString) mkString(", ")
      s"${this.className}($modelDescs)"
    }

    def liftModels(f: (ModelSpec, Seq[Tree]) => Seq[Tree]) = {
      models.liftModels(f)
    }

    def modelsFlat = models.flatten.toList.drop(1)
  }

  object ModelsSpec
  {
    implicit object ModelsSpecMonoid
    extends Monoid[ModelsSpec]
    {
      val zero = ModelsSpec(ModelSpec.zeroTree, Nil,
        BodyData.BodyDataMonoid.zero)

      def append(a: ModelsSpec, b: => ModelsSpec) = {
        ModelsSpec(
          a.models ⊹ b.models,
          a.enums ⊹ b.enums,
          a.body ⊹ b.body
        )
      }
    }
  }

  trait ModelsParser
  {
    def apply(body: BodyData) = {
      implicit val info = BasicInfo(body)
      val classes = body.classes filterNot(_.isCase)
      val mspec = info.modelTree cobind(parseModel)
      val (enums, modules) = body.modules partition(_.isEnum)
      val espec = enums map(EnumSpec.apply)
      val parsed = body.copy(classes = classes, modules = modules)
      ModelsSpec(mspec, espec, parsed)
    }

    def parseModel(node: BasicModelTree)
    (implicit info: BasicInfo): ModelSpec = {
      node.rootLabel match {
        case ModelTreeRoot => implicitly[Monoid[ModelSpec]].zero
        case m: ModelTreeModel => {
          val c = m.cls
          val loc = info.modelTree.nodeLoc(node)
          val attrs = c.fields map(a => processField(loc, a))
          val classes = c.body.classes.filterNot(_.isCase)
          ModelSpec(c, attrs, c.bases, c.body.copy(classes = classes), loc,
            None, None)
        }
      }
    }

    def processField(container: ModelLoc, field: FieldData)
    (implicit info: BasicInfo) = {
      val model = info.modelFor(AttrSpec.actualType(field.tpt))
        .map(info.modelTree.nodeLoc)
      val enum = info.isEnum(field.tpt)
      createAttr(field, enum, model, Some(container))
    }

    val dummyTree = ModelTreeNode.root.node().loc

    def createAttr(
      field: FieldData,
      enum: Boolean,
      model: Option[ModelLoc],
      container: Option[ModelLoc]
    ) = {
      val eff = AttrSpec.effectiveType(field.tpt)
      model some[AttrSpecBase] { m =>
        val embed = AttrSpec.isEmbed(field.tpt)
        val list = AttrSpec.isList(field.tpt)
        if (embed) {
          val embedder = container
            .getOrAbort(s"Need container for embedded attr $field")
          if (list) EmbeddedListSpec(field, eff, m, embedder)
          else EmbeddedSingleSpec(field, eff, m, embedder)
        }
        else {
          if (list) RefListSpec(field, eff, m)
          else RefSingleSpec(field, eff, m)
        }
      } none {
        if (enum) EnumAttrSpec(field, eff, field.default)
        else AttrSpec(field, eff, field.default)
      }
    }

    def parseValDef(
      valDef: ValDef,
      enum: Boolean = false,
      model: Option[ModelLoc] = None
    ) = {
      val field = FieldData.parse(valDef)
      createAttr(field, enum, model, None)
    }

    def attrFromData(
      name: TermName, tpt: TypePath,
      model: Option[ModelLoc] = None
    ) = {
      parseValDef(q"val $name: $tpt", false, model)
    }

    def relocate(spec: ModelSpec, tree: BasicModelTree, loc: ModelTreeLoc) = {
      val relocParams = spec.params map { par =>
        val modelLoc = par match {
          case h: HasClass => tree modelLoc(h)
          case _ => None
        }
        DefaultModelsParser.createAttr(
          par.field, par.isEnum, modelLoc, Some(loc))
      }
      ModelSpec(spec.cls, relocParams, spec.bases, spec.body, loc,
        Some(spec.basic), spec.path)
    }
  }

  object DefaultModelsParser
  extends ModelsParser

  def modelsParser = DefaultModelsParser

  case class ModelImpl(model: ModelSpec, cls: ClassData, module: ModuleData,
    extra: List[Tree])
  {
    def lift = cls.lift :: module.lift :: extra
  }

  object ModelImpl
  {
    def parse(model: ModelSpec, cls: Tree, module: Tree, extra: Tree*) = {
      ModelImpl(model, ClassData.parse(cls), ModuleData.parse(module),
        extra.toList)
    }

    implicit val seqLifter = new SeqLifter[ModelImpl] {
      def liftSeq(m: ModelImpl) = m.lift
    }

    implicit object `ModelImpl EmbedLifter`
    extends BodyEmbedLifter[ModelImpl]
    {
      def embedLens = GenLens[ModelImpl](_.module) ^|-> ModuleData.body
    }
  }

  /**
   * Lifts the elements of a tree, starting at the leafs.
   * Intermediate nodes are lifted via an implicit [[EmbedLifter]], to which
   * the lifted trees of the current node's children are supplied for proper
   * embedding.
   */
  implicit def `Tree Liftable with embedding`[A](implicit el: EmbedLifter[A]) =
    new Liftable[STree[A]] {
      def apply(tree: STree[A]) = {
        tree liftSub(el.liftSub) flattenTrees
      }
    }

  implicit class `Embedder tree`[A](tree: STree[A])
  (implicit el: EmbedLifter[A]) {
    def lift = tree.subForest
  }

  implicit class `Tree Seq`(trees: Seq[Tree])
  {
    def flattenTrees = q"..$trees"
  }
}
