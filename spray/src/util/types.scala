package tryp.spray.meta

import scalaz._, Scalaz._

trait SprayTypes
{
  type Embeddings = NonEmptyList[tryp.mongo.EmbeddingI]
  type Result = ValidationNel[String, String]
  type MongoDb = reactivemongo.api.DefaultDB
}
