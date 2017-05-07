package tryp.spray

import _root_.spray.routing.Route

import concurrent.ExecutionContext

import tryp.mongo.DefinedPath

trait Model[A <: Model[A]]
extends tryp.mongo.Model[A]
{ self: A =>

  def routes(name: String, path: DefinedPath)
  (implicit ec: ExecutionContext, db: MongoDb): Route
}

trait ModelParams[A <: tryp.app.ModelT]
{
  def embeddedNames: List[String]
}
