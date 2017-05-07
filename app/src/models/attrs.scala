package tryp.app

import scalaz._, Scalaz._

trait AttrMetadata
extends tryp.macros.Annotation
{
  import c.universe._

  val idField = true

  val idType = tq"ObjectId"
  val idTerm = q"ObjectId"

  case class EnumSpec(module: ModuleBase)
  {
    def term = module.term
    def body = module.body
    def tpe = module.tpe
    def name = module.ident
  }

  trait AttrSpecBase
  {
    def tpt: Tree
    def field: FieldData

    def term = field.name

    def mods = field.mods

    def name = term.toString

    def tpe = tpt.typeName

    def fTypes = tpt match {
      case tq"$f[..$tparams]" => f -> tparams
      case _ => abort(s"Bad type tree in $this: $tpt")
    }

    def isFType = tpt match {
      case tq"$_[..$_]" => true
      case _ => false
    }

    lazy val option = tpt match {
      case tq"Option[..$_]" => true
      case _ => false
    }

    def isOptional = field.hasAnnotation("opt")

    def actualType = TypeName(tpt.toString).left[Select]

    def actualTerm = actualType.toTermName

    lazy val valDef = q"$mods val $term: $tpt = $default"

    def ngValDef = q"val $term: $ngType = $ngDefault"

    def ngType = tpt

    def ngDefault = default

    def jsValDef = q"val $term: $jsType = $jsDefault"

    def jsType = tpt

    def jsDefault = default

    def default: Tree = option ? (q"None": Tree) | q""

    def isEnum = false
  }

  type Attrs = List[AttrSpecBase]

  trait SimpleAttrSpec
  extends AttrSpecBase
  {
    def customDefault: Tree

    override def default = {
      if (customDefault != q"") customDefault
      else super.default
    }
  }

  case class AttrSpec(field: FieldData, tpt: Tree, customDefault: Tree)
  extends SimpleAttrSpec

  case class EnumAttrSpec(field: FieldData, tpt: Tree, customDefault: Tree)
  extends SimpleAttrSpec
  {
    val enumTerm =
      tpt match {
        case tq"${name: TermName}.Value" => name
        case tq"${name: TypeName}" => name.toTermName
        case _ => term
      }

    override def jsType = tq"String"

    override def jsDefault = q"$default.toString"

    override def ngType = tq"String"

    override def ngDefault = q"$default.toString"

    override def isEnum = true
  }

  trait ModelAttrSpec
  extends AttrSpecBase
  with HasClass
  {
    def model: ModelLoc

    def cls = model.getLabel.cls

    override def actualType = model.modelTypePath
  }

  trait SingleModelSpec
  extends ModelAttrSpec
  {
    override def ngType = tq"String": Tree

    override def ngDefault = q"""""""": Tree

    override def jsType = tq"String": Tree

    override def jsDefault = q"""""""": Tree
  }

  trait RefSpec
  extends ModelAttrSpec

  trait EmbeddedSpec
  extends ModelAttrSpec
  {
    def container: ModelLoc

    def parentTerms = container.parentTerms

    def modelPathTerms = container.modelPathTerms

    def modelTermPath = container.modelTermPath
  }

  // FIXME move ng/js overrides to impl
  case class RefSingleSpec(field: FieldData, tpt: Tree, model: ModelLoc)
  extends SingleModelSpec
  with RefSpec

  case class EmbeddedSingleSpec(field: FieldData, tpt: Tree, model: ModelLoc,
    container: ModelLoc)
  extends SingleModelSpec
  with EmbeddedSpec

  trait ModelListSpec
  extends ModelAttrSpec
  {
    override def default = q"List()": Tree

    override def ngType = tq"List[String]": Tree

    override def ngDefault = q"List()": Tree

    override def jsType = tq"js.Array[String]": Tree

    override def jsDefault = q"js.Array()": Tree
  }

  case class RefListSpec(field: FieldData, tpt: Tree, model: ModelLoc)
  extends ModelListSpec
  with RefSpec


  case class EmbeddedListSpec(field: FieldData, tpt: Tree, model: ModelLoc,
    container: ModelLoc)
  extends ModelListSpec
  with EmbeddedSpec

  trait SyntheticAttrSpec
  extends SimpleAttrSpec
  {
    def desc: String

    lazy val field =
      FieldData.parse(q"val ${TermName(desc)}: $tpt = $customDefault")
  }

  case class IdSpec()
  extends SyntheticAttrSpec
  {
    def desc = "id"

    def tpt = idType

    def customDefault = q"$idTerm.generate"
  }

  case class DateSpec(desc: String)
  extends SyntheticAttrSpec
  {
    def tpt = tq"DateTime"

    def customDefault = q"DateTime.now"
  }

  object AttrSpec
  {
    def actualType[A >: Tree with TypeName](input: A): TypePath = {
      val tpt = input match {
        case n: TypeName => n.tree
        case t: Tree => t
      }
      val act = tpt match {
        case tq"Option[..$act]" => act.headOption
        case tq"Embed[List[..$act]]" => act.headOption
        case tq"Embed[..$act]" => act.headOption
        case tq"Contain[List[..$act]]" => act.headOption
        case tq"Contain[..$act]" => act.headOption
        case tq"List[..$act]" => act.headOption
        case tq"$act" => Some(act)
      }
      act map(_.tp) getOrElse {
        c.abort(c.enclosingPosition,
          s"AttrSpec.actualType: Strange input: $input -> $tpt")
      }
    }

    def isList(tpe: Tree) = {
      tpe match {
        case tq"Embed[List[..$act]]" => true
        case tq"Contain[List[..$act]]" => true
        case tq"List[..$act]" => true
        case _ => false
      }
    }

    def isEmbed(tpe: Tree) = {
      tpe match {
        case tq"Embed[List[..$act]]" => true
        case tq"Embed[..$act]" => true
        case tq"Contain[List[..$act]]" => true
        case tq"Contain[..$act]" => true
        case _ => false
      }
    }

    def isContain(tpe: Tree) = {
      tpe match {
        case tq"Embed[List[..$act]]" => true
        case tq"Embed[..$act]" => true
        case tq"Contain[List[..$act]]" => true
        case tq"Contain[..$act]" => true
        case _ => false
      }
    }

    def effectiveType(tpe: Tree) = {
      tpe match {
        case tq"Embed[..$act]" => act.head
        case tq"Contain[..$act]" => act.head
        case tq"$act" => act
      }
    }
  }

  implicit object `AttrSpecBase Liftable`
  extends Liftable[AttrSpecBase]
  {
    def apply(m: AttrSpecBase) = m.valDef
  }
}
