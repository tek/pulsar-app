package tryp
package app

import scalaz._, Scalaz._

trait ModelInterfaces
extends ResourceMetadata
{
  import c.universe._

  trait AttrOpsBase
  {
    def attr: AttrSpecBase
    def tpt = attr.tpt
    def term = attr.term
    def name = attr.name
    def mods = attr.mods
    def default = attr.default
    def isOptional = attr.isOptional

    def ctorParam = q"val $ctorTerm: $ctorTpt = $default"

    def ctorArg = ctorTerm

    def ctorTerm = term

    def ctorTpt: Tree = tpt

    def ctorAscription = q"$ctorTerm: $ctorTpt"

    def ctorAssign = q"$ctorTerm = $ctorTerm"
  }

  trait Ops
  {
    trait SimpleAttrOpsBase
    extends AttrOpsBase

    trait IdOpsBase
    extends AttrOpsBase

    trait DateOpsBase
    extends AttrOpsBase

    trait ModelAttrOpsBase
    extends AttrOpsBase

    trait SingleModelOpsBase
    extends ModelAttrOpsBase

    trait ModelListOpsBase
    extends ModelAttrOpsBase
    {
      def attr: ModelListSpec
    }

    trait RefOpsBase
    extends ModelAttrOpsBase

    trait EmbeddedOpsBase
    extends ModelAttrOpsBase
    {
      def attr: EmbeddedSpec

      def attrPathTerm = attr.modelTermPath :+ term.u
    }

    trait RefSingleOpsBase
    extends RefOpsBase
    with SingleModelOpsBase
    {
      override def ctorTpt: Tree = idType
    }

    trait RefListOpsBase
    extends RefOpsBase
    with ModelListOpsBase
    {
      override def ctorTpt = tq"List[${idType}]"

      def monadWithIdType = {
        attr.fTypes match {
          case (cont, tparam :: Nil) => tq"$cont[$idType]"
          case a =>
            abort(s"Invalid ref list type, need monad (got $a)")
        }
      }
    }

    trait EmbeddedSingleOpsBase
    extends EmbeddedOpsBase
    with SingleModelOpsBase

    trait EmbeddedListOpsBase
    extends EmbeddedOpsBase
    with ModelListOpsBase
    {
      def attr: EmbeddedListSpec

      def monadWithModelType = {
        attr.fTypes match {
          case (cont, tparam :: Nil) => tq"$cont[$tparam]"
          case a =>
            abort(s"Invalid embbeded list type, need monad (got $a)")
        }
      }
    }

    def attrBase(a: AttrSpecBase) =
      new AttrOpsBase { def attr = a }

    implicit def attr(a: AttrSpec) =
      new SimpleAttrOpsBase { def attr = a }

    implicit def id(a: IdSpec) =
      new IdOpsBase { def attr = a }

    implicit def date(a: DateSpec) =
      new DateOpsBase { def attr = a }

    implicit def refSingle(a: RefSingleSpec) =
      new RefSingleOpsBase { def attr = a }

    implicit def refList(a: RefListSpec) =
      new RefListOpsBase { def attr = a }

    implicit def embeddedSingle(a: EmbeddedSingleSpec) =
      new EmbeddedSingleOpsBase { def attr = a }

    implicit def embeddedList(a: EmbeddedListSpec) =
      new EmbeddedListOpsBase { def attr = a }
  }

  object BasicOps
  extends Ops

  trait ModelsTransformerBase
  {
    val ops: Ops = BasicOps

    def opsToOpt[A <: AttrOpsBase: ClassTag](spec: AttrSpecBase) = {
      attrSpecOps(spec) match {
        case a: A => Some(a)
        case _ => None
      }
    }

    def opsTo[A <: AttrOpsBase: ClassTag](spec: AttrSpecBase) = {
      opsToOpt[A](spec) match {
        case Some(a) => a
        case _ => abort(s"Couldn't convert $spec to ${className[A]}")
      }
    }

    trait ResourceOpsBase
    {
      def res: ResourceSpec

      def tpe = res.tpe

      def bases = res.bases

      def name = tpe.toString

      def params = res.params

      def modelFields: Attrs = params

      def ctorParams = modelFields map(_.ctorParam)

      lazy val ctorArgs = modelFields map(_.ctorArg)

      def plural = res.name.plural(0)

      def embeddedNames = res.embeddedParams map(_.name)

      def hasEmbed = !res.embeddedParams.isEmpty

      def typePath = res.basic.modelTypePath

      def termPath = res.basic.modelTermPath
    }

    implicit def attrSpecOps(a: AttrSpecBase): AttrOpsBase = {
      a match {
        case r: RefSingleSpec => ops.refSingle(r)
        case r: RefListSpec => ops.refList(r)
        case e: EmbeddedSingleSpec => ops.embeddedSingle(e)
        case e: EmbeddedListSpec => ops.embeddedList(e)
        case i: IdSpec => ops.id(i)
        case d: DateSpec => ops.date(d)
        case a: AttrSpec => ops.attr(a)
        case _ => ops.attrBase(a)
      }
    }

    implicit def resourceSpecOps(cls: ResourceSpec): ResourceOpsBase =
      new ResourceOpsBase { def res = cls }

    abstract class ModelInterface(m: ModelSpec)
    {
      def queryCommon = {
        List(
          q"""
          def create(..${m.ctorParams})($iec) =
            insert(${m.term}(..${m.ctorArgs}))
          """
        )
      }

      def create: ModelImpl
    }
  }
}
