package tryp
package slick
package sync

import scalaz._, Scalaz._

sealed trait SyncStatus
{
  def up: Boolean
  def label: String
  def msg: String

  def prefix = s"$sign $label".blue

  def sign = if(up) "↑" else "↓"

  def repr = prefix + s" $msg"
}

case class SimpleSyncStatus(label: String, msg: String, up: Boolean)
extends SyncStatus

case class SyncModelStatus(label: String, msg: String, record: Model,
  up: Boolean)
extends SyncStatus

case class NoPendingActions(label: String)
extends SyncStatus
{
  def up = true
  def msg = ""
}

case class SendSyncStatus(label: String, data: String, action: String)
extends SyncStatus
{
  def up = true
  def msg = s"$action: $data"
}

case class FetchSyncStatus(label: String, data: String)
extends SyncStatus
{
  def up = false
  def msg = data
}

case class EmbeddedIdStatus[A: Show](container: Model, attr: String, result: A)
extends SyncStatus
{
  def up = false
  def label = s"${container.show}/$attr/ids"
  def msg = result.show.toString
}

trait SyncResultOps
{
  implicit class StringToSyncResult(s: String) {
    def syncSuccess(msg: String, up: Boolean) =
      SimpleSyncStatus(s, msg, up).success.toDbio

    def syncFail(msg: String, up: Boolean) =
      SimpleSyncStatus(s, msg, up).failure.toDbio

    def syncModelFail(msg: String, record: Model, up: Boolean) =
      SyncModelStatus(s, msg, record, up).failure.toDbio

    def syncModelSuccess(msg: String, record: Model, up: Boolean) =
      SyncModelStatus(s, msg, record, up).success.toDbio

    def syncNoPending =
      NoPendingActions(s).success.toDbio

    def updateSuccess(data: String) =
      SendSyncStatus(s, data, "update").success.toDbio

    def addSuccess(data: String) =
      SendSyncStatus(s, data, "add").success.toDbio

    def deleteSuccess(id: ObjectId) =
      SendSyncStatus(s, id.stringify, "delete").success.toDbio

    def fetch(data: String) =
      FetchSyncStatus(s, data)
  }

  implicit def showIds[F[_]: Traverse] = new Show[F[ObjectId]]
  {
    override def show(ids: F[ObjectId]) = {
      ids.toList map(_.stringify) mkString("(", ",", ")")
    }
  }

  implicit class ActionStatusOps[A](a: AnyAction[A])(implicit ec: EC)
  {
    def status(both: SyncStatus) = statusFS(both, both)

    def statusFS(fail: => SyncStatus, success: => SyncStatus) =
      a andThen(success.success.toDbio) cleanUp(_ => fail.failure.toDbio)
  }
}

trait SyncResultInstances
{
  implicit def showSyncResults[F[_]: Traverse] = new Show[F[SyncResult]]
  {
    override def show(res: F[SyncResult]) = {
      res
        .toList
        .collect {
          case scalaz.Success(status: NoPendingActions) =>
            " ○ ".yellow + status.repr
          case scalaz.Success(status) =>
            " ✓ ".green + status.repr
          case scalaz.Failure(status) =>
            " ✗ ".red + status.repr
        }
        .mkString("\n")
    }
  }
}

object SyncResult
extends SyncResultOps
with SyncResultInstances
