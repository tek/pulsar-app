package tryp
package slick
package sync

@export
trait Exports
{
  type ModelTableI[A <: Model] = slick.ModelTableI[A]
}

trait All
{
  val PendingActions = tryp.slick.sync.schema.PendingActions

  type AnySyncQuery = SyncQueryI[
    A,
    _ <: ModelTable[A],
    _ <: BackendMapper[A]
  ] forSome { type A <: SyncModel }
  type SyncResult = scalaz.Validation[SyncStatus, SyncStatus]
  type ResultsAction = AnyAction[Traversable[SyncResult]]
}

@integrate(slick)
object `package`
extends All
