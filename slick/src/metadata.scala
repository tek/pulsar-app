package tryp
package slick
package sync

import _root_.slick.lifted._
import _root_.slick.jdbc.JdbcProfile

import tryp.slick._

trait EmbeddedAttrMetadataI
{
  val query: AnySyncQuery
}

abstract class EmbeddedAttrMetadata[P <: JdbcProfile](implicit profile: P)
extends TableMetadataI[P]
with ProfileConcern[P]
with EmbeddedAttrMetadataI
{
  def path: String
  val name = path
  override val query: AnyTableQuery with AnySyncQuery
}

abstract class ModelMetadata[A <: Model, P <: JdbcProfile](
  implicit val profile: P)
{
  def embeddedFields: List[EmbeddedAttrMetadataI]
}

case class SyncTableMetadata(name: String, query: AnySyncQuery)
{
  def pendingActionsKey = query.path
}

trait SyncSchemaMetadataI
{
  def tables: Map[String, SyncTableMetadata]
}

trait SyncSchema
extends Schema
{
  val syncMetadata: SyncSchemaMetadataI

  // implicit val pending: PendingActions.Slick
  val pending = new schema.PendingActions.Slick

  lazy val pendingActions = pending.PendingActionSet

  // access the pending action set of each query to initialize missing tables
  def initPending() = syncMetadata.tables mapValues { _.query.pending }

  override def initDb()(implicit dbInfo: DbInfo, ec: EC) = {
    metadata.createMissingTables()
    pending.metadata.createMissingTables()
    initPending()
  }

  def dropDb()(implicit dbInfo: DbInfo, ec: EC) = {
    metadata.dropAll()
    pending.metadata.dropAll()
  }

  def resetDb()(implicit dbInfo: DbInfo, ec: EC) = {
    dropDb()
    initDb()
  }
}
