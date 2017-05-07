package tryp
package spray

import reflect.classTag

import scalaz._
import Scalaz._

import com.github.limansky.mongoquery.reactive._

import _root_.spray.http._
import StatusCodes._
import _root_.spray.routing._
import _root_.spray.json._
import _root_.spray.httpx.marshalling._
import _root_.spray.json.DefaultJsonProtocol._
import _root_.spray.httpx.SprayJsonSupport._
import shapeless._

import tryp.mongo._
import BsonProtocol._

case class CrudParams(deep: Boolean, end: Boolean, query: Map[String, String])

trait Routing
extends Directives
{
  def crudParams(end: Boolean) = {
    parameterMap map { params =>
      val deep = params.get("deep")
      val rest1 = params - "deep"
      CrudParams(deep.isDefined, end, rest1)
    }
  }
}

class CrudRouter[
A <: Model[A]: JsonFormat: ClassTag,
B <: ModelCreator[A],
C <: ModelUpdater[A]
]
(adapter: CrudParams => DbAdapter[A])
(implicit marshalling: Marshalling[A, B, C], ec: EC, mongo: MongoDb)
extends Routing
{
  import marshalling._
  import BasicJson._

  def name = classTag[A].className

  def dbAdapter(end: Boolean) = {
    crudParams(end) map(adapter)
  }

  implicit def futureModelResponse[D: ToResponseMarshaller](fut: Future[D]) = {
    onComplete(fut) {
      case util.Success(a) =>
        complete(a)
      case util.Failure(e) =>
        complete(InternalServerError, e.getMessage)
    }
  }

  implicit def futureResultResponse(fut: Future[Result]) = {
    onComplete(fut) {
      case util.Success(scalaz.Success(a)) =>
        complete(a)
      case util.Success(scalaz.Failure(e)) =>
        complete(UnprocessableEntity, e.toList.mkString(", "))
      case util.Failure(e) =>
        complete(InternalServerError, e.getMessage)
    }
  }

  def routes(
    implicit postValidator: Validator[B], putValidator: Validator[C]
  ): Route = {
    dbAdapter(true) { db =>
      pathEnd {
        post {
          entity(marshalling.post) { data =>
            postValidator(data).apply {
              dynamic {
                db.create(data.model)
              }
            }
          }
        } ~
        get {
          dynamic {
            db.filter()
          }
        }
      } ~
      get {
        path("find") {
          dynamic {
            db.one()
          }
        } ~
        path("ids") {
          dynamic {
            db.ids()
          }
        }
      } ~
      pathPrefix(Segment) { id =>
        pathEnd {
          put {
            entity(marshalling.put) { data =>
              putValidator(data).apply {
                dynamic {
                  db.updateById(ObjectId(id), data)
                }
              }
            }
          } ~
          get {
            dynamic {
              db.oneById(ObjectId(id))
            }
          }
        }
      }
    } ~ dbAdapter(false) { db =>
      pathPrefix(Segment / Segment) { (id, attr) =>
        onComplete(db.oneById(ObjectId(id))) {
          case util.Success(Some(container)) =>
            container.routes(attr, db.subPath(container))
          case util.Success(None) =>
            complete(
              InternalServerError,
              s"Couldn't find $name with id $id for routing attr $attr"
            )
          case util.Failure(e) =>
            complete(InternalServerError, e.getCause)
        }
      }
    }
  }
}
