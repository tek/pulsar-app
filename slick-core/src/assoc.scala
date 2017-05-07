package tryp
package slick

// import tryp.slick.meta.ModelQueryExt

trait AssocAdapterI
{
  def replace(ids: Traversable[ObjectId])
  (implicit ec: EC): AnyAction[Traversable[_]]
}

trait AssocProfile
{
  self: Profile =>

  import profile.api._

  case class AssocTable[A <: Model, B <: Model](
    tag: Tag, name: String, toQuery: ModelTableQuery[B])
  (implicit fromQuery: ModelTableQuery[A])
  extends profile.Table[AssocIds](tag, name)
  {
    def fromId = column[ObjectId]("from_id")
    def toId = column[ObjectId]("to_id")
    def from = foreignKey("from", fromId, fromQuery)(_.id)
    def to = foreignKey("to", toId, toQuery)(_.id)
    def * = (fromId, toId)
  }

  class AssocTableQuery[A <: Model: ModelTableQuery, B <: Model: ModelTableQuery]
  (name: String, toQuery: ModelTableQuery[B])
  extends TableQuery(t => new AssocTable[A, B](t, name, toQuery))

  // FIXME clean up types
  case class AssocAdapter[A <: Model, B <: Model, C <: Table[B]](
    id: ObjectId,
    assocQuery: AssocTableQuery[A, B],
    otherQuery: CrudQuery[B, C],
    hook: Boolean
  )
  (implicit ownerQuery: Dao[A])
  extends ModelQueryExt
  with AssocAdapterI
  {
    def query = for {
      x <- assocQuery
      if x.fromId === id
      y <- otherQuery
      if x.toId === y.id
    } yield y

    def map[F, G, T](f: C => F)
    (implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]) = {
      query map(f)
    }

    def all = query.result

    def one = query.one

    def byId(id: ObjectId) = query.byId(id)

    def idsQuery = for {
      x <- assocQuery
      if x.fromId === id
    } yield x.toId

    def ids = idsQuery.result

    def addIds(newIds: Traversable[ObjectId])(implicit ec: EC) = {
      modified andThen {
        ids flatMap { current =>
          val diff = newIds.toSet &~ current.toSet
          assocQuery ++= diff.map((id, _: ObjectId))
        }
      }
    }

    def add(other: ObjectId)(implicit ec: EC) = {
      ids flatMap { current =>
        if(current contains ids) nopDbio
        else assocQuery += (id, other)
      }
    }

    def insert(record: C#TableElementType)(implicit ec: EC) = {
      otherQuery.insert(record) andThen(add(record.id)) map(_ => record)
    }

    def remove(ids: Traversable[ObjectId]) = {
      val assoc = for {
        x <- assocQuery
        if x.fromId === id && x.toId.inSet(ids)
      } yield x
      modified andThen assoc.delete
    }

    // TODO add Deletion for other
    // FIXME uncomment
    def delete(ids: Traversable[ObjectId]) = {
      val other = for {
        x <- otherQuery
        if x.id.inSet(ids)
      } yield x
      modified andThen {
        queryDeleteActionExtensionMethods(other).delete andThen remove(ids)
      }
    }

    def assocs = assocQuery filter(_.fromId === id)

    def otherIds(implicit ec: EC) = {
      assocs
        .map(_.toId)
        .!
        .map(_.toSet)
    }

    def replace(newIds: Traversable[ObjectId])(implicit ec: EC) = {
      modified
        .andThen(assocs filterNot(_.toId inSet(newIds)) delete)
        .andThen {
        otherIds
          .map(newIds.toSet.diff)
          .flatMap(_ map(add) toDbioSeq)
      }
    }

    def modified: DbAction = {
      if (hook) ownerQuery.modified(id)
      else nopDbio
    }
  }

}
