package tryp.lift

import reflect.macros._

import scalaz._, Scalaz._

import net.liftweb.record.field._

import tryp.app._

trait Rest[A <: Rest[A]]
extends net.liftmodules.restrecord.RestRecord[A]
{
  self: A =>

    object id extends StringField(this, "")

    def restId: String = id.value

    def prefix: RestPath = Nil

    override def recordPath = modelPath :+ restId
}

trait RestMeta[A <: Rest[A]]
extends net.liftmodules.restrecord.RestMetaRecord[A]
{
  self: A =>

    override val jsonExcludeFields = List("id")
}

trait RestRecord
extends ModelsBase
{
  import c.universe._

  object RestRecordTransformer
  extends ModelsTransformer
  {
    sealed trait Embedding
    object Embedding {
      case object No extends Embedding
      case object Record extends Embedding
      case object Meta extends Embedding
    }

    implicit class RrModelOps(m: ModelSpec)(implicit models: ModelsSpec)
    {
      def plural = m.name.plural(0)

      def listType = TypeName(plural)

      def listTerm = TermName(plural)

      def accessorTerm = listTerm.d

      def suffixType(s: String) = m.tpe.suffix(s)

      def modelType = m.tpe

      def modelTerm = m.term

      def path = plural.toLowerCase

      val idField = IdSpec()

      val attrsWithoutId = m.params

      val attrs = attrsWithoutId :+ idField

      def model = {
        q"""
        class $modelType
        extends Rest[$modelType]
        {
          container =>

          def meta = $modelTerm

          def modelPath = prefix :+ $path

          ..$recordFields

          object embed {
            ..$recordEmbeds
          }
        }
        """
      }

      def embeddedModels = m.embeddedParams map(models.modelForAttr)

      def recordEmbeds = embeddedModels map(_.meta(Embedding.Record))

      def recordFields = {
        val custom = m.modules.map { o =>
            o.term.toString -> o
        }.toMap
        attrs filterNot(_.embed) collect {
          case param @ RefSpec(name, _, _) =>
            custom.get(name.toString) some(_.reify) none(
              defaultRefField(param))
          case param @ RefListSpec(name, _, _) =>
            custom.get(name.toString) some(_.reify) none(
              defaultRefListField(param))
          case param @ EnumAttrSpec(_, _, _) =>
            custom.get(param.enumTerm.toString) some(_.reify) none(
              defaultEnumField(param))
          case param @ AttrSpec(name, _, _) =>
            custom.get(name.toString) some(_.reify) none(defaultField(param))
        }
      }

      val fieldTypes = Map(
        "DateTime" -> TypeName("JodaTimeField")
        )

      def defaultField(param: AttrSpec) = {
        val fieldType = fieldTypes.get(param.tpe.toString) getOrElse {
          param.tpe.suffix("Field")
        }
        q"""
        object ${param.term}
        extends $fieldType(this, implicitly[Monoid[${param.tpt}]].zero)
        """
      }

      def defaultEnumField(param: EnumAttrSpec) =
        q"""
        object ${param.term}
        extends EnumNameField(this, ${param.enumTerm})
        """

      def defaultRefField(param: RefSpec) = {
        if (param.embed) embeddedField(param, tq"JSONSubRecordField")
        else
          q"""
          object ${param.term}
          extends StringField(this, "")
          """
      }

      def defaultRefListField(param: RefListSpec) = {
        if (param.embed) embeddedField(param, tq"JSONSubRecordArrayField")
        else
          q"""
          object ${param.term}
          extends JSONBasicArrayField[$modelType, JString](this)
          """
      }

      def embeddedField(param: AttrSpecBase, tpe: Ident) = {
        q"""
        object ${param.term}
        extends $tpe[$modelType, ${param.tpe}](this, ${param.tpe.toTermName})
        """
      }

      def meta(nest: Embedding, parentMeta: Option[TermName] = None): Tree = {
        val prefix = nest match {
          case Embedding.Record => q"override def prefix = container.recordPath"
          case Embedding.Meta =>
            val parent = parentMeta getOrElse(
              sys.error("nested meta needs parent"))
            q"override def prefix = $parent.recordPath"
          case Embedding.No => q""
        }
        val embeds = embeddedModels map(_.meta(Embedding.Meta, Some(m.term)))
        q"""
        object $modelTerm
        extends $modelType
        with RestMeta[$modelType]
        with RestConfig
        {
          ..$prefix

          override def instantiateRecord = new $modelType {
            ..$prefix
          }

          override def recordPath = modelPath :+ *

          ..$embeds
        }
        """
      }

      def rest = List(
        model,
        meta(Embedding.No)
      )

      def accessor = {
        q"""
        var $accessorTerm: $listType = js.native
        """
      }
    }

    val doubleMonoid = {
      q"""
      implicit object `Double Monoid` extends Monoid[Double]
      {
        def zero = 0.0
        def append(a: Double, b: => Double) = a + b
      }
      """
    }

    val datetimeMonoid = {
      q"""
      implicit object `DateTime Monoid` extends Monoid[DateTime]
      {
        def zero = 0L.toDateTime
        def append(a: DateTime, b: => DateTime): DateTime = a
      }
      """
    }

    val monoids = Seq(
      doubleMonoid,
      datetimeMonoid
    )

    val imports = Seq(
      q"import net.liftweb.record._",
      q"import net.liftweb.record.field._",
      q"import net.liftweb.record.field.joda._",
      q"import scalaz._",
      q"import Scalaz._",
      q"import scalaz.syntax.std.option._",
      q"import net.liftweb.json.JString",
      q"import com.github.nscala_time.time.Imports._",
      q"import tryp.lift._"
    )

    def apply(spec: ModelsSpec) = {
      implicit val s = spec
      val models = (spec.models map(_.rest) flatten)
      spec.mergeTree {
        q"""
        object Rest
        {
          ..$imports
          ..$monoids
          ..$models
        }
        """
      }
    }
  }
}
