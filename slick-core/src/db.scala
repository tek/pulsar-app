package tryp
package slick

import _root_.slick.lifted._
import _root_.slick.profile._
import _root_.slick.jdbc.meta.MTable
import _root_.slick.jdbc.JdbcProfile

trait ProfileConcern[P <: JdbcProfile]
{
  implicit val profile: P
  type AnyTableQuery = TableQuery[_ <: profile.Table[_]]
}

class HasProfile[P <: JdbcProfile](implicit val profile: P)
extends ProfileConcern[P]

trait JdbcConcern
{
  val profile: JdbcProfile
}

trait DbInfo
extends JdbcConcern
{
  def url = s"jdbc:$name:$dbLoc"

  def name: String

  def dbLoc: String

  def db(): profile.Backend#DatabaseDef

  override def toString = s"${this.className}($url)"
}

trait SqliteDbInfo
extends DbInfo
{
  val profile = SqliteDbInfo.profile

  import profile.api._

  def name = "sqlite"

  def driver = "org.sqlite.JDBC"

  def db() = Database.forURL(url, driver = driver)
}

object SqliteDbInfo
{
  implicit val profile = _root_.slick.jdbc.SQLiteProfile
}

case class TestFileDbInfo(file: String)
extends SqliteDbInfo
{
  def dbLoc = s"target/$file.db"
}

case class DroidDbInfo(dbLoc: String)
extends SqliteDbInfo
{
  override def name = "sqldroid"

  override def driver = "org.sqldroid.SQLDroidDriver"
}

abstract class TableMetadataI[P <: JdbcProfile](implicit val profile: P)
{
  import profile.api._

  val name: String
  val query: TableQuery[_ <: profile.Table[_]]

  def ddl = query.schema

  def empty(implicit db: DbInfo, ec: EC) = {
    MTable.getTables(name).get().isEmpty
  }

  def create = ddl.create

  def drop = ddl.drop
}

// FIXME remove effect
abstract class SchemaMetadataI[P <: JdbcProfile](implicit val profile: P)
{
  def tables: Map[String, TableMetadataI[P]]

  def createMissingTables()(implicit db: DbInfo, ec: EC) = {
    tables
      .collect { case (name, table) if(table.empty) => table.create }
      .toDbioSeq
      .get()
  }

  def dropAll()(implicit db: DbInfo, ec: EC) = {
    tables
      .collect { case (name, table) => Try(table.drop.get()) }
  }
}

abstract class Schema(implicit val profile: JdbcProfile)
extends JdbcConcern
{
  val dbLock = new Object
  val metadata: SchemaMetadataI[profile.type]

  def initDb()(implicit dbInfo: DbInfo, ec: EC): Unit = {
    metadata.createMissingTables()
  }
}

private[slick] trait TrypQuery
[A <: Model, B <: ModelTable[A]]
{
  self: TableQuery[B] =>

  type Model = A
  type Table = B

  def all: AnyAction[Seq[A]]

  def ids: Query[Rep[ObjectId], ObjectId, Seq]

  def update(obj: A)(implicit ec: EC): AnyAction[A]

  def delete(obj: A): DbAction

  def byId(id: ObjectId): AnyAction[Option[A]]

  def insert(obj: A)(implicit ec: EC): AnyAction[A]

  def modified(id: ObjectId): DbAction
}
