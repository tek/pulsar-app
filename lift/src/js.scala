package tryp.lift

import reflect.macros._

import scalaz._, Scalaz._

import tryp.app._

trait Js
extends ModelsBase
{
  import c.universe._

  object JsTransformer
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

    implicit class JsModelOps(m: ModelSpec)(implicit models: ModelsSpec)
    {
      def plural = m.name.plural(0)

      def listType = TypeName(plural)

      def listTerm = TermName(plural)

      def accessorTerm = listTerm.d

      def suffixType(s: String) = TypeName(s"${m.tpe}$s")

      def modelType = m.tpe

      def modelTerm = m.term

      def serviceName = modelType.suffix("Service")

      val idField = ObjectIdSpec()

      val attrsWithoutId = m.params

      val attrs = attrsWithoutId :+ idField

      def jsParams = attrs map(_.jsValDef)

      def embeddedModels = m.embeddedParams map(models.modelForAttr)

      def model = {
        q"""
        class $modelType(..${jsParams})
        extends Model
        """
      }

      def modelList = {
        q"""
        class $listType(items: js.Array[$modelType])
        extends ModelHolder(items)
        """
      }

      def scalajs = List(
        model,
        modelList
      )

      def accessor = {
        q"""
        var $accessorTerm: $listType = js.native
        """
      }

      def service(prefix: String = "") = {
        q"""
        class ${serviceName.prefix(prefix)}
        extends ModelService[$modelType, $listType]
        """
      }

      def serviceInjectionParam(prefix: String = "") = {
        q"""
        val ${modelTerm.prefix(prefix)}: ${serviceName.prefix(prefix)}
        """
      }

      def adapter(prefix: String = "") = {
        q"""
        class ${modelType.prefix(prefix).suffix("Adapter")}(
          ${serviceInjectionParam(prefix)})
        extends ModelAdapter[$modelType, $listType]
        {
          val service = ${modelTerm.prefix(prefix)}
        }
        """
      }

      def adapterServiceDecl(prefix: String = "") = {
        q"""
        module.serviceOf[${modelType.prefix(prefix).suffix("Adapter")}]
        """
      }
    }

    val imports = Seq(
      q"import scalajs.js",
      q"import js.annotation.JSExportAll",
      q"import biz.enef.angulate._"
    )

    def bases = {
      q"""
      class Model
      extends js.Object

      @JSExportAll
      case class ModelRequestParams(limit: Int, skip: Int,
        ids: js.Array[String], extra: Seq[(String, String)])

      implicit def `ModelRequestParams to Dynamic`(m: ModelRequestParams) =
        m.extra.foldLeft(
        js.Dynamic.literal("limit" -> m.limit, "skip" -> m.skip, "ids" -> m.ids)
        ) {
          case (dyn, (par, value)) =>
            dyn.updateDynamic(par)(value)
            dyn
        }

      class ModelHolder[A <: Model](val items: js.Array[A])
      extends js.Object

      class ModelService[A <: Model, B <: ModelHolder[A]]
      extends js.Object
      {
        def query(params: js.Object): core.QPromise[B] = js.native
        def search(params: js.Object): core.QPromise[B] = js.native
        def create(params: A): core.QPromise[A] = js.native
      }

      @JSExportAll
      abstract class ModelAdapter[A <: Model, B <: ModelHolder[A]]
      extends Service
      {
        var items: js.Array[A] = js.Array()

        val service: ModelService[A, B]

        val setter: js.Function1[B, js.Any] = (b: B) => {
          items = b.items
          b
        }

        def fetch(params: (String, String)*) = {
          service.query(param(extra = params)).`then`(setter, error)
        }

        def fetch(ids: js.Array[String], params: (String, String)*) = {
          service.query(param(ids = ids, extra = params)).`then`(setter, error)
        }

        val error: js.Function1[js.Any, Unit] = { a: js.Any =>
          println ("error querying the model service")
        }

        def param(
          limit: Int = 0, skip: Int = 0,
          ids: js.Array[String] = js.Array(),
          extra: Seq[(String, String)] = Seq()
        ) =
            ModelRequestParams(limit, skip, ids, extra)
      }
      """
    }

    def modelScope(implicit spec: ModelsSpec) = {
      val models = spec.models map(_.accessor)
      q"""
      trait ModelScope
      extends Scope
      {
        ..$models
      }
      """
    }

    def operator(model: ModelSpec)(implicit models: ModelsSpec) = {
      model.embeddedModels map(_.service("Operator"))
    }

    def operatorInjections(model: ModelSpec)(implicit models: ModelsSpec) = {
      model.embeddedModels map(_.serviceInjectionParam("Operator"))
    }

    def opAdapter(model: ModelSpec)(implicit models: ModelsSpec) = {
      model.embeddedModels map(_.adapter("Operator"))
    }

    def operatorAdapterServiceDecls(model: ModelSpec)
    (implicit models: ModelsSpec) = {
      model.embeddedModels map(_.adapterServiceDecl("Operator"))
    }

    def modelService(implicit spec: ModelsSpec) = {
      val services = spec.models map(_.service())
      val injectedServices = spec.models map(_.serviceInjectionParam())
      val adapters = spec.models map(_.adapter())
      val user = spec.modelMap.get("User")
      val op = user.map(operator) getOrElse(Nil)
      val opInjections = user.map(operatorInjections) getOrElse(Nil)
      val opAdapters = user.map(opAdapter) getOrElse(Nil)
      val adapterServiceDecls = spec.models map(_.adapterServiceDecl())
      val opAdapterServiceDecls =
        user.map(operatorAdapterServiceDecls) getOrElse(Nil)
      q"""
      lazy val module = angular.createModule("tryp_js")

      ..$services
      ..$op

      class ModelsService(..$injectedServices, ..$opInjections)
      extends Service

      ..$adapters
      ..$opAdapters
      ..$adapterServiceDecls
      ..$opAdapterServiceDecls
      """
    }

    def apply(spec: ModelsSpec) = {
      implicit val ms = spec
      val models = (spec.models map(_.scalajs) flatten)
      val scope = modelScope(spec)
      Log.d("merging js extensions")
      spec.mergeTree {
        q"""
        object Js {
          ..$imports
          ..$bases
          ..$models
          $scope
          ..$modelService
        }
        """
      }
    }
  }
}
