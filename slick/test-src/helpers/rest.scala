package tryp.slick.test

import scalaz._, Scalaz._

import tryp.slick._
import tryp.slick._
import tryp.slick.sync._

class TestRestClient(responses: Seq[String])
extends RestClient
{
  val it = Iterator(responses: _*)

  val requests = scala.collection.mutable.Buffer[(String, String)]()

  def response(req: Request) = {
    val body = req.body getOrElse("{}")
    requests += (req.path -> body)
    if (it.hasNext) it.next.right else "No response left".left
  }

  def post(req: Request) = response(req)
  def put(req: Request) = response(req)
  def delete(req: Request) = response(req)
  def get(req: Request) = response(req)
}
