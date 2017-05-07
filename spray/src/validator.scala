package tryp
package spray

import scalaz._
import Scalaz._

import _root_.spray.routing._

abstract class Validator[-A]
{
  def apply[B <: A](model: B)
  (implicit ec: ExecutionContext, db: MongoDb): Directive0 =
    Directives.onSuccess(validate(model))
      .flatMap { v =>
        val message = v match {
          case scalaz.Failure(l) => l.toList.mkString(", ")
          case _ => "no error"
        }
        Directives.validate(v.isSuccess, message)
      }

  def validate[B <: A](model: B)(implicit ec: ExecutionContext, db: MongoDb):
    ScalaFuture[Result]

  val allRight = "he's going to be all right".successNel[String]

  def failIfDefined(o: Option[_], message: String) = {
    o map(_ => message.failureNel[String]) getOrElse(allRight)
  }
}

object DefaultValidator
extends Validator[Any]
{
  def validate[B <: Any](model: B)
  (implicit ec: ExecutionContext, db: MongoDb) =
    ScalaFuture(allRight)

  override def toString = "fallback validator"
}

trait ValidatorInstances
{
  implicit def validator[A]: Validator[A] = DefaultValidator
}

object Validator
extends ValidatorInstances
