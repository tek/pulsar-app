package tryp
package slick
package sync

import io.circe._

trait SyncModel
extends Model
{
  def simpleJson: Json
  def json(implicit ec: EC): AnyAction[Json]
}
