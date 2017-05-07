package tryp
package spray

import _root_.spray.json._

import reactivemongo._
import api._
import bson._

import com.github.limansky.mongoquery.reactive._

import scalaz._, Scalaz._

import tryp.mongo._
import BsonProtocol._
import BsonTypes._
import ResultFunctions._

abstract class DbAdapter[A <: tryp.mongo.Model[A]: BSONDocumentReader]
(implicit modelParams: ModelParams[A], modelDao: CollectionDao[A], ec: EC)
{
  val crudParams: CrudParams

  def builder: QueryBuilder[A]

  def subPath(container: A): DefinedPath

  def one() = builder.one()

  def oneById(id: ObjectId) = builder.byId(id).one()

  def filter() = builder.filter()

  def ids() = builder.ids()

  implicit class ValidatorSuccess[A](res: ScalaFuture[ValidationNel[String, A]])
  {
    def mapSuccess(f: A => ScalaFuture[Result]) = {
      res.flatMap(_.fold({ nel => ScalaFuture.successful(nel.failure[String]) }, f))
    }
  }

  def create(data: A): ScalaFuture[Result] = {
    modelDao.validateInsert(data) mapSuccess(createUnchecked)
  }

  protected def createUnchecked(data: A): ScalaFuture[Result]

  def updateById(attrId: ObjectId, data: ModelUpdater[A]) = {
    val updater = updateByIdUnchecked(attrId, (_: BSONDocument))
    modelDao.validateUpdate(data) mapSuccess(updater)
  }

  protected def updateByIdUnchecked(attrId: ObjectId, data: BSONDocument)
  : ScalaFuture[Result]

  def addRefId(record: ObjectId, name: String, id: ObjectId) = {
    val upd = BSONDocument(name -> id.toBSON)
    updateByIdUnchecked(record, mq"{ $$addToSet: $upd }")
  }

  lazy val query = {
    if (crudParams.end) queryDoc
    else BSONDocument()
  }

  // TODO verify that when the adapter is at an intermediate position, the
  // embedded records aren't necessary for routing
  def exclude = {
    if (crudParams.deep && crudParams.end)
      BSONDocument()
    else
      BSONDocument(modelParams.embeddedNames map(_ -> 0L.toBSON))
  }

  def queryDoc = {
    val list = """\(((?:\w+,?)+)\)""".r
    val params = crudParams.query
      .map { case (k, v) =>
        val q = v match {
          case list(content) =>
            val strings = content.split(',').toSeq
            if (k == "id") {
              val ids = strings flatMap(s => Try(ObjectId(s)) toOption)
              mq"{ $$in: $ids }"
            }
            else
              mq"{ $$in: $strings }"
          case _ =>
            BSONString(v)
        }
        k -> (q: BSONValue)
      }
    params.toMap.toBSONDoc
  }
}

// TODO support single embedded and referenced attrs
// @param name: the parameter name of the embedded array in the container
// @param path: the sequence of embedding records
case class EmbeddedAttrAdapter
[A <: Model[A]: BSONDocTypeMapper: ModelParams: Dao]
(name: String, crudParams: CrudParams, path: DefinedPath)
(implicit ec: EC)
extends DbAdapter[A]
{
  def dao = path.dao

  def createUnchecked(data: A) = {
    builder.create(data)
  }

  def updateByIdUnchecked(attrId: ObjectId, data: BSONDocument) = {
    builder.updateById(attrId, data)
  }

  def builder = dao.query(query).project(exclude).embed(name, path)

  def subPath(container: A) = {
    val current = (Embedding(name, container, this): EmbeddingI).wrapNel
    path match {
      case RootPath(dao, base, _) =>
        EmbeddedPath(dao, base, current)
      case EmbeddedPath(dao, base, wrappers) =>
        EmbeddedPath(dao, base, wrappers ⊹ current)
    }
  }
}

case class RefAttrAdapter[A <: Model[A]: BSONDocumentReader: ModelParams]
(name: String, targetIds: List[ObjectId], crudParams: CrudParams,
  path: DefinedPath)
(implicit dao: Dao[A], ec: EC)
extends DbAdapter[A]
{
  def createUnchecked(data: A) = {
    import tryp.Awaiting._
    dao.insert(data) flatMap { r =>
      val container = path match {
        case r: RootPath => r
        case EmbeddedPath(_, _, wrappers) =>
          wrappers.last
      }
      container.adapter.addRefId(container.id, name, data.id)
    } toResult
  }

  def updateByIdUnchecked(attrId: ObjectId, data: BSONDocument) = {
    if (targetIds.contains(attrId))
      builder.byId(attrId).updateRaw(data) toResult
    else Result.errF(s"id $attrId not present in refs for update with $data")
  }

  def builder = dao.query(query).project(exclude).refs(targetIds, path)

  def subPath(container: A) = {
    RootPath(dao, container, this)
  }
}

case class FlatAdapter[A <: Model[A]: BSONDocumentReader: ModelParams]
(crudParams: CrudParams)
(implicit dao: Dao[A], ec: EC)
extends DbAdapter[A]
{
  def createUnchecked(obj: A) = {
    dao.insert(obj) toResult
  }

  def updateByIdUnchecked(attrId: ObjectId, data: BSONDocument) = {
    dao.updateById(attrId, data) toResult
  }

  def builder = dao.query(query).project(exclude)

  def subPath(container: A) = RootPath(dao, container, this)
}
