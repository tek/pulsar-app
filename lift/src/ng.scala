package tryp.lift

import concurrent.ExecutionContext

import reflect.macros._

import scalaz._, Scalaz._

import net.liftweb.common._
import net.liftmodules.ng._
import Angular._
import FutureConversions._

import tryp.app._

case class ModelRequestParams(limit: Int = 0, skip: Int = 0,
  ids: List[String] = List(), extra: List[(String, String)] = List())
extends NgModel
{
  def query = ("limit" -> limit.toString) :: ("skip" -> skip.toString) :: extra
}

case class ModelCreateParams[A](ids: List[String] = List(), data: A)
extends NgModel

object GenericRestFacade
{
  def query[A <: Rest[A], B <: NgModel]
  (rest: RestMeta[A], converter: List[A] => B, params: ModelRequestParams)
  (implicit ec: ExecutionContext) = {
    rest.search(params.ids, params.query) map { response =>
      val result = response match {
        case net.liftweb.common.Failure(_, Full(exc), _) => throw exc
        case Full(a) => a
        case _ => sys.error("Failed to query backend")
      }
      converter(result)
    }
  }

  def search[A <: Rest[A], B <: NgModel]
  (rest: RestMeta[A], converter: List[A] => B, params: ModelRequestParams)
  (implicit ec: ExecutionContext) = query(rest, converter, params)

  def create[A <: Rest[A]](record: A)(implicit ec: ExecutionContext) = {
    record.create() map { response =>
      response match {
        case net.liftweb.common.Failure(_, Full(exc), _) => throw exc
        case Full(a) => a
        case _ => sys.error("Failed to query backend")
      }
    }
  }
}

trait Ng
extends ModelsBase
{
  import c.universe._

  object NgTransformer
  extends ModelsTransformer
  {
    case class ObjectIdSpec()
    extends SimpleAttrSpec
    {
      def tpt = tq"String"

      def customDefault = q""" "" """

      val field = FieldData.parse {
        q"""
        val id: $tpt = $customDefault
        """
      }
    }

    implicit class ModelOps(m: ModelSpec)(implicit models: ModelsSpec)
    {
      def plural = m.name.plural(0)

      def listType = TypeName(plural)

      def listTerm = TermName(plural)

      def modelType = m.tpe

      def modelTerm = m.term

      def binderType = modelType.suffix("Binder")

      val idField = ObjectIdSpec()

      val attrsWithoutId = m.params

      val attrs = attrsWithoutId :+ idField

      def ngParams = attrs map(_.ngValDef)

      def embeddedModels = m.embeddedParams map(models.modelForAttr)

      def adapter = {
        q"""
        case class $modelType(..$ngParams)
        """
      }

      def adapterList = {
        q"""
        case class $listType(items: List[$modelType])
        extends NgModel
        """
      }

      def adapterListComp = {
        q"""
        object $listTerm
        {
          def all = List()
        }
        """
      }

      def binder = {
        q"""
        class $binderType
        extends SimpleNgModelBinder[$listType](
          ${plural.d},
          $listTerm($listTerm.all),
          identity,
          1000
        )
        with BindingToClient
        with BindingToServer
        with SessionScope
        """
      }

      def angular = List(
        adapter,
        adapterList
      )

      def factoryTerm = listTerm.d

      def converterTerm = TermName(s"convert$listTerm")

      def creatorTerm = listTerm.prefix("create")

      def converter = {
        val args = attrs filterNot(_.embed) collect {
          case s @ RefListSpec(_, _, _) =>
            q"${s.term} = item.${s.term}.value map(_.toString)"
          case s @ RefSpec(_, _, _) =>
            q"${s.term} = item.${s.term}.value.toString"
          case s @ EnumAttrSpec(_, _, _) =>
            q"${s.term} = item.${s.term}.value.toString"
          case s @ ObjectIdSpec() =>
            q"${s.term} = item.${s.term}.value.toString"
          case s @ AttrSpec(_, _, _) =>
            q"${s.term} = item.${s.term}.value"
        }
        q"""
        def $converterTerm(items: List[Rest.${m.tpe}]): $listType = {
          $listTerm(items map(item => ${m.term}(..$args)))
        }
        """
      }

      def factory(prefix: List[TermName] = Nil,
        paramTransform: Tree = q"params"
      ): Tree = {
        val current = prefix :+ modelTerm
        val name = current.mkString
        val fact = current.foldLeft(q"RestFacade": Tree) {
          (sel, name) => q"$sel.$name"
        }
        q"""
        mod.factory($name,
          jsObjFactory()
            .future("query", { par: ModelRequestParams =>
              $fact.query(par).la })
            .future("search", { par: ModelRequestParams =>
              $fact.search(par).la })
            .future("create", { par: ModelCreateParams[$modelType] =>
              $fact.create(par).la })
        )
        ..${embeddedFactories(current)}
        """
      }

      def embeddedFactories(prefix: List[TermName]) = {
        attrs filter(_.embed) map(models.modelForAttr) map(
          _.factory(prefix))
      }

      def creator(meta: Tree) = {
        val args = attrs filterNot(_.embed)
        args.foldLeft(q"$meta.createRecord": Tree) { (sel, arg) =>
          arg match {
            case spec @ RefListSpec(_, _, _) =>
              q"$sel.${spec.term}(data.${spec.term} map(JString.apply))"
            case spec @ EnumAttrSpec(f, _, _) =>
              q"$sel.${f.term}(${spec.enumTerm}.withName(data.${f.term}))"
            case spec =>
              q"$sel.${spec.term}(data.${spec.term})"
          }
        }
      }

      def restFactories(
        prefix: List[TermName] = Nil,
        paramTransform: Tree = q"params"
      ): Tree = {
        val current = prefix :+ modelTerm
        val meta = current.foldLeft(q"Rest": Tree) {
          (sel, name) => q"$sel.$name"
        }
        val createContainer = prefix.zipWithIndex.foldLeft(q"Rest": Tree) {
          case (sel, (name, index)) =>
            q"""
            $sel.$name.createRecord.id(ids.lift($index).getOrElse("*")).embed
            """
        }
        val createMeta = q"$createContainer.$modelTerm"
        q"""
        object $modelTerm
        {
          def query(params: ModelRequestParams) = {
            val par: ModelRequestParams = $paramTransform
            GenericRestFacade.query($meta, $converterTerm, par)
          }

          def search(params: ModelRequestParams) = {
            val par: ModelRequestParams = $paramTransform
            GenericRestFacade.search($meta, $converterTerm, par)
          }

          def create(params: ModelCreateParams[$modelType]) = {
            val par: ModelCreateParams[$modelType] = $paramTransform
            val ids = par.ids
            val data = par.data
            val record = ${creator(createMeta)}
            GenericRestFacade.create(record)
          }

          ..${embeddedRestFactories(current, paramTransform)}
        }
        """
      }

      def embeddedRestFactories(
        prefix: List[TermName],
        paramTransform: Tree = q"params"
      ) = {
        attrs filter(_.embed) map(models.modelForAttr) map(
          _.restFactories(prefix))
      }
    }

    val imports = {
      Seq(
        q"import net.liftweb.common._",
        q"import net.liftmodules.ng._",
        q"import Angular._",
        q"import FutureConversions._",
        q"import net.liftweb.json.JsonAST._",
        q"import concurrent.ExecutionContext.Implicits.global",
        q"import org.bson.types.ObjectId"
      )
    }

    def genericRestFactories(implicit spec: ModelsSpec) = {
      q"""
      """
    }

    val opIdTransform = {
      q"""
      operatorId some { id =>
        params.copy(ids = id :: params.ids)
      } none sys.error("no current user for ng query")
      """
    }

    def restFacade(implicit spec: ModelsSpec) = {
      val factories = spec.models.map(_.restFactories())
      q"""

      object RestFacade
      {
        ..$factories
        $opRestFacade
      }
      """
    }

    val opModel = "User"

    val opName = "Operator"

    def opEmbed(implicit spec: ModelsSpec) =
      spec.modelMap get(opModel) map(_.embeddedModels) getOrElse(Nil)

    val opPrefix = List(TermName(opModel))

    def opRestFacade(implicit spec: ModelsSpec) = {
      val factories = opEmbed.map(_.restFactories(prefix = opPrefix,
        paramTransform = opIdTransform))
      q"""
      object Operator
      {
        ..$factories
      }
      """
    }

    def modelsService(implicit spec: ModelsSpec) = {
      val factories = spec.models map(_.factory())
      val opFactories = opEmbed map(_.factory(prefix = List(TermName(opName))))
      q"""
      trait ModelsService
      {
        def render = renderIfNotAlreadyDefined(
          angular.module("models") tap { mod =>
            ..$factories
            ..$opFactories
          }
        )
      }
      """
    }

    def apply(spec: ModelsSpec) = {
      implicit val ms = spec
      val ng = (spec.models map(_.angular) flatten)
      val converters = spec.models.map(_.converter)
      Log.d("merging ng extensions")
      spec.mergeTree {
        q"""
        object Ng {
          ..$imports
          ..$ng
          ..$converters
          ..$restFacade
          ..$modelsService
        }
        """
      }
    }
  }
}
