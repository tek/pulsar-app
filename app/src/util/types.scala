package tryp.app.meta

import scalaz.NonEmptyList

import reactivemongo.bson.BSONObjectID

trait ModelTypes
{
  type ObjectId = BSONObjectID
  type Ids = List[ObjectId]

  val ObjectId = BSONObjectID
}

object ModelTypes
extends ModelTypes
