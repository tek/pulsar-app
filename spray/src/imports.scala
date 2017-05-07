package tryp.spray

object ModelImports
extends tryp.app.meta.Globals
with BasicJson
with tryp.mongo.BsonTypes
with tryp.mongo.BsonProtocol
with scalaz.syntax.ToShowOps
{
  val Directives = spray.routing.Directives
}
