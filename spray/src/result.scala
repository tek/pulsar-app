package tryp
package spray

import scalaz._, Scalaz._

object Result
extends _root_.spray.json.DefaultJsonProtocol
{
  def ok = "operation successful".successNel[String]
  def err(message: String) = message.failureNel[String]
  def errF(message: String) = ScalaFuture.successful(err(message))
}

object ResultFunctions
{
  implicit class FutureToResult[A](f: Future[A])(implicit ec: EC) {
    def toResult = {
      f.map(r => Result.ok).recover { case e => Result.err(e.getMessage) }
    }
  }
}
