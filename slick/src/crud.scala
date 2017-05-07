package tryp
package slick
package sync

import io.circe._

import scalaz._, Scalaz._

import _root_.slick.lifted._
import _root_.slick.driver._

import tryp.slick.{Profile, TrypQuery, TableMetadataI, HasProfile}
import schema.Action
import SyncResult._

trait SyncQueryI
[A <: SyncModel, B <: ModelTable[A], Mapper <: BackendMapper[A]]
extends TrypQuery[A, B]
{
  self: TableQuery[B] =>

  val pendingActionsSchema: PendingActions.Slick

  import pendingActionsSchema.Exports._

  def name: String
  def path: String
  def jsonForIds(ids: Traversable[ObjectId]): AnyAction[Traversable[String]]
  def completeSync(a: Action): AnyAction[_]
  def completeDeletion(a: Deletion): AnyAction[_]
  def sync(rest: RestAdapter, attr: String = name): ResultsAction
  def pending: AnyAction[PendingActionSet]
  def insertUnrecorded(obj: A): AnyAction[A]
  def updateUnrecorded(obj: A): AnyAction[A]
  def all: AnyAction[Seq[A]]
  def updateAssocs(obj: A, mapper: Mapper)(implicit ec: EC): AnyAction[_]
  def deleteByIdsUnrecorded(ids: Traversable[ObjectId]): AnyAction[_]
  def idExists(id: ObjectId): AnyAction[Boolean]
}

trait SyncProfile
extends Profile
{
  import profile.api._

  object Types
  extends HasProfile[profile.type]

  case class SyncSchemaMetadata(tables: Map[String, SyncTableMetadata])
  extends SyncSchemaMetadataI

  case class EmbeddedAttrMetadata(path: String,
    query: Types.AnyTableQuery with AnySyncQuery)
  extends tryp.slick.sync.EmbeddedAttrMetadata[profile.type]

  abstract class SyncCrud
  [
    A <: Model: ClassTag,
    B <: Table[A] { type TableElementType = A }: ClassTag
  ]
  (cons: Tag => B)(implicit ec: EC)
  extends CrudQuery[A, B](cons)
  {
    val baseQuery = this: TrypQuery[A, B] with CrudQuery[A, B]

    def path: String

    def name: String

    val pendingActionsSchema: PendingActions.Slick

    import pendingActionsSchema.Exports._

    def pending: AnyAction[PendingActionSet] = {
      new QueryResultOps (
        PendingActionSet
          .filter(_.model === path)
        )
        .oneOr(PendingActionSet.create(path))
    }

    override def insert(obj: B#TableElementType)(implicit ec: EC) = {
      for {
        sets <- pending
        insertedObj <- insertUnrecorded(obj)
        insertedAddition <- Addition.create(insertedObj.id)
        _ <- sets.additions.add(insertedAddition.id)
      } yield insertedObj
    }

    def insertUnrecorded(obj: A) = {
      super.insert(obj)
    }

    override def update(obj: A)(implicit ec: EC) = {
      for {
        _ <- recordUpdate(obj.id)
        ret <- super.update(obj)
      } yield ret
    }

    def recordUpdate(id: ObjectId) = {
      for {
        sets <- pending
        u <- Update.create(id)
        _ <- sets.updates.add(u.id)
      } yield ()
    }

    def updateUnrecorded(obj: A) = {
      super.update(obj)
    }

    override def delete(obj: A) = {
      for {
        sets <- pending
        a <- Deletion.create(obj.id)
        _ <- sets.deletions.add(a.id)
      } yield ()
      super.delete(obj)
    }

    def deleteByIdsUnrecorded(ids: Traversable[ObjectId]) = {
      this.byIdsQ(ids).delete
    }
  }

  abstract class SyncQuery[
    A <: SyncModel: ClassTag,
    B <: Table[A] { type TableElementType = A }: ClassTag,
    Mapper <: BackendMapper[A]: Encoder: Decoder
  ]
  (cons: Tag => B)
  (implicit ec: EC)
  extends SyncCrud[A, B](cons)
  with SyncQueryI[A, B, Mapper]
  {
    val pendingActionsSchema: PendingActions.Slick

    import pendingActionsSchema.Exports._

    // need these here as this class is inherited by the companion
    implicit def jsonEncoder: Encoder[A]
    implicit def jsonDecoder: Decoder[A]
    implicit def modelMetadata: ModelMetadata[A, profile.type]

    def jsonForIds(ids: Traversable[ObjectId]) = {
      baseQuery byIds(ids) flatMap { _.map(_.json map(_.spaces2)).toDbioSeq }
    }

    def completeSync(a: Action) = {
      pending flatMap { pa =>
        DBIO.seq(
          pa.additions.delete(List(a.id)),
          pa.updates.delete(List(a.id))
        )
      }
    }

    def completeDeletion(a: Deletion) = {
      pending flatMap { _.deletions.delete(List(a.id)) }
    }

    def sync(rest: RestAdapter, attr: String = name) = {
      BackendSyncTask[A, B, Mapper](this, rest / attr).process
    }
  }
}
