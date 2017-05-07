package tryp.app

trait ModelT
{
  def id: ObjectId
}

trait PathComponent
{
  def path: String
  def id: ObjectId
}

case class SimplePathComponent(path: String, id: ObjectId)
extends PathComponent
