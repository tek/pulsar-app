package tryp
package app

@export
trait Exports
{
  type ObjectId = reactivemongo.bson.BSONObjectID
  type Ids = List[ObjectId]

  val ObjectId = reactivemongo.bson.BSONObjectID
}

object `package`
extends Exports
