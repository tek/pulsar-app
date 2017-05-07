package tryp
package slick

import _root_.slick.lifted._
import _root_.slick.profile._
import _root_.slick.jdbc.JdbcProfile
// import _root_.slick.driver._

import scalaz._, Scalaz._

import com.github.nscala_time.time.Imports.DateTime

trait Profile
extends AssocProfile
// with meta.ModelQueryExt
{ pro =>
  val profile: JdbcProfile

  import profile.api._

  type Table[A <: Model] = profile.Table[A] with ModelTableI[A]
  type AnyTableQuery = TableQuery[_ <: profile.Table[_]]

  case class TableMetadata(name: String, query: AnyTableQuery)
  extends TableMetadataI[profile.type]

  case class SchemaMetadata(tables: Map[String, TableMetadataI[profile.type]])
  extends SchemaMetadataI[profile.type]

  implicit class QueryDeleteExtension[A <: Model, C[_]]
  (q: Query[_ <: Table[A], A, C])
  {
    def deleteById(id: ObjectId) = q.filter(_.id === id).delete
  }

  abstract class CrudQuery
  [A <: Model: ClassTag, B <: Table[A]: ClassTag]
  (cons: Tag => B)
  extends TableQuery[B](cons)
  with TrypQuery[A, B]
  {
    def withDate(obj: A, u: DateTime): A

    override def toString = s"${this.className}(${className[B]})"

    def ids = map(_.id)

    def update(obj: A)(implicit ec: EC): AnyAction[A] = {
      filter(_.id === obj.id) update(obj) map(_ => obj)
    }

    def delete(obj: A) = this.deleteById(obj.id)

    def byIdQ(id: ObjectId) = filter(_.id === id)

    def byId(id: ObjectId) = byIdQ(id).one

    def byIdsQ(ids: Traversable[ObjectId]) = filter(_.id inSet(ids))

    def byIds(ids: Traversable[ObjectId]) = byIdsQ(ids).result

    def idExists(id: ObjectId) = filter(_.id === id).exists.result

    def byIdEither(id: ObjectId)(implicit ec: EC) = {
      filter(_.id === id).one
        .map { _.map(\/-(_)) getOrElse(-\/("Not found")) }
    }

    def insert(obj: A)(implicit ec: EC): AnyAction[A] = {
      (this += obj) map(_ => obj)
    }

    def all = this.result
  }
}
