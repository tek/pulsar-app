package tryp
package slick
package sync

import _root_.slick.jdbc.JdbcProfile

trait BackendMapper[A <: Model]
extends ModelT
{
  def model: A
}

case class MapperSyncer[
A <: SyncModel,
B <: ModelTable[A],
C <: BackendMapper[A]
]
(query: SyncQueryI[A, B, C], mapper: C)
(implicit ec: EC, profile: JdbcProfile)
{
  def sync(): AnyAction[A] = {
    for {
      exists <- query.idExists(mapper.id)
      result <- if (exists) update() else create()
    } yield result
  }

  def update() = {
    apply(query.updateUnrecorded)
  }

  def create() = {
    apply(query.insertUnrecorded)
  }

  def apply(app: A => AnyAction[A]) = {
    app(mapper.model)
  }
}
