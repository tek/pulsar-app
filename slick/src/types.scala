// package tryp.slick.sync.meta

// import tryp.slick.sync.{BackendMapper, SyncModel, SyncStatus, SyncQueryI}
// import tryp.slick.meta.SlickTypes._

// trait SyncTypes
// {
//   type Model = tryp.slick.Model
//   type NamedModel = tryp.slick.NamedModel
//   type SyncResult = scalaz.Validation[SyncStatus, SyncStatus]
//   type ResultsAction = AnyAction[Traversable[SyncResult]]
//   type AnySyncQuery = SyncQueryI[
//     A,
//     _ <: ModelTable[A],
//     _ <: BackendMapper[A]
//   ] forSome { type A <: SyncModel }
//   type DbInfo = tryp.slick.DbInfo
// }

// object SyncTypes
// extends SyncTypes
