package tryp
package slick
package sync
package schema

import concurrent.ExecutionContext.Implicits.global

import _root_.slick.jdbc.JdbcProfile

trait Action
extends tryp.slick.ModelT
{
  def target: ObjectId
}

object Action
{
  implicit class ActionSeq(seq: Iterable[Action]) {
    def targets = seq map { _.target }
  }
}
import Action._

@tryp.annotation.Slick()
object PendingActions
{
  case class Addition(target: ObjectId)
  extends Action

  case class Update(target: ObjectId)
  extends Action

  case class Deletion(target: ObjectId)
  extends Action

  case class PendingActionSet(model: String, additions: List[Addition],
    updates: List[Update], deletions: List[Deletion])
    {
      def updatesWithoutAdditions = {
        for {
          add <- additions.query.map(_.target).result
          ups <- updates.all if !add.contains(ups)
        } yield ups
      }
    }

  class Slick
  {
    object PendingActionSet
    {
      def attr(name: String) = filter(_.model === name).one
    }
  }
}
