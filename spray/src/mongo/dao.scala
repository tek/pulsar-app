package tryp
package mongo

import scalaz._
import Scalaz._

import _root_.spray.json._

import com.github.nscala_time.time.Imports.DateTime

import reactivemongo._
import api._
import bson.{BSONDocument, BSONDocumentReader, BSONArray, BSONDouble,
  BSONDocumentWriter, BSONValue, BSONInteger}
import collections.bson.{BSONCollection, BSONQueryBuilder}
import commands.bson.BSONCountCommand.Count
import commands.bson.BSONCountCommandImplicits._

import com.github.limansky.mongoquery.reactive._

import tryp.app._
import tryp.spray._
import BasicJson._
import BsonTypes._
import BsonProtocol._

trait Model[A <: Model[A]]
extends ModelT
{
  self: A =>

  def dao(implicit db: MongoDb): CollectionDao[A]

  def update()(implicit ec: EC, db: MongoDb) = {
    dao.update(this)
    this
  }
}

trait ModelLoc
{
  def record: ModelT
  def adapter: DbAdapter[A] forSome { type A <: Model[A] }
  def id: ObjectId
}

trait EmbeddingI
extends PathComponent
{
  def adapter: DbAdapter[_]
}

case class Embedding[A <: Model[A]](
  path: String, record: A, adapter: DbAdapter[A])
extends EmbeddingI
with ModelLoc
{
  def id = record.id
}

sealed trait DbPath

abstract sealed trait DefinedPath
extends DbPath
{
  def dao: CollectionDao[_ <: ModelT]
  def record: ModelT
  def id = record.id
}

object DefinedPath
{
  def unapply(path: DefinedPath) = Some((path.dao, path.id))
}

case object EmptyPath
extends DbPath

case class RootPath(dao: CollectionDao[_ <: ModelT], record: ModelT,
  adapter: DbAdapter[A] forSome { type A <: Model[A] })
extends DefinedPath
with EmbeddingI
with ModelLoc
{
  val path = "/"
}

case class EmbeddedPath(dao: CollectionDao[_ <: ModelT], record: ModelT,
  embed: Embeddings)
extends DefinedPath

case class EmptyRecord(id: ObjectId)
extends ModelT

trait DaoBase[M <: ModelT]
{
  final def insert(m: M)(implicit ec: EC): ScalaFuture[M] =
    for {
      inserted <- insertImpl(prePersist(m))
      postFetched <- postFetch(inserted)
    } yield {
      postPersist(postFetched)
    }

  final def update(m: M)(implicit ec: EC): ScalaFuture[M] =
    for {
      updated <- updateImpl(prePersist(m))
      postFetched <- postFetch(updated)
    } yield {
      postPersist(postFetched)
    }

  def removeById(id: ObjectId)
  (implicit ec: EC): ScalaFuture[commands.WriteResult]

  protected def allImpl(implicit ec: EC): ScalaFuture[List[M]]

  final def all(implicit ec: EC): ScalaFuture[List[M]] =
    fetchMany(allImpl)

   protected def fetch1(f: => ScalaFuture[Option[M]])
   (implicit ec: EC): ScalaFuture[Option[M]] = f flatMap {
    case Some(m) => postFetch(m) map { Option(_) }
    case None => ScalaFuture.successful(None)
  }

  protected def fetchMany(f: => ScalaFuture[List[M]])
  (implicit ec: EC): ScalaFuture[List[M]] = f flatMap { ms =>
    ScalaFuture.traverse(ms)(postFetch)
  }

  protected def insertImpl(m: M)(implicit ec: EC): ScalaFuture[M]
  protected def updateImpl(m: M)(implicit ec: EC): ScalaFuture[M]

  protected def prePersist(m: M)(implicit ec: EC): M = m
  protected def postPersist(m: M)(implicit ec: EC): M = m

  protected def postFetch(m: M)(implicit ec: EC): ScalaFuture[M] =
    ScalaFuture.successful(m)
}

trait QueryHelpers
{
  val emptyQuery = BSONDocument()

  def idQuery[A: BVWriter](id: A) = mq"{ _id: $id }"

  def multiIdQuery(ids: Ids) = idQuery(mq"{ $$in: $ids }")
}

case class QueryData(
  collection: BSONCollection, params: BSONDocument, projection: BSONDocument)

class QueryBuilder[A <: ModelT: BSONDocumentReader]
(implicit data: QueryData, ec: EC)
extends QueryHelpers
{
  def clone(implicit data: QueryData) = new QueryBuilder[A]

  def apply(params: BSONDocument) = {
    addParams(params)
  }

  def addParams(params: BSONDocument) = {
    clone(data.copy(params = data.params ++ params))
  }

  def project(projection: BSONDocument) = {
    projectOnly(data.projection ++ projection)
  }

  def projectOnly(projection: BSONDocument) = {
    clone(data.copy(projection = projection))
  }

  def refs(ids: Ids, path: DbPath) = {
    new RefQueryBuilder(ids, path)
  }

  def embed[B <: Model[B]: BSONDocTypeMapper](
    name: String, path: DefinedPath) =
      new EmbedQueryBuilder[B](name, path)

  def collection = data.collection

  def one() = build.one[A]

  def filter() = find().collect[List]()

  def find() = cursor()

  def query = data.params

  def projection = data.projection

  def build = collection.find(query, projection)

  def cursor() = build.cursor[A]()

  def byId(id: ObjectId) = addParams(idQuery(id))

  def byIds(ids: Ids) = addParams(multiIdQuery(ids))

  def ids() = {
    projectOnly(idQuery(1L)).build
      .cursor[EmptyRecord]()
      .collect[List]()
      .map(_.map(_.id))
  }

  def extractIds(v: Option[BSONValue]) = {
    v collect {
      case BSONArray(docs) => docs collect {
        case scala.util.Success(a @ BSONDocument(_)) => a.as[EmptyRecord].id
      } toList
    } getOrElse(Nil)
  }

  def create(data: A): ScalaFuture[Result] = ???

  def update(data: ModelUpdater[A]) = updateRaw(data.bson)

  def updateRaw(data: BSONDocument) = {
    collection.update(query, mq"{ $$set : $data }", multi = true)
  }
}

class RefQueryBuilder[A <: ModelT: BSONDocumentReader](
  targetIds: Ids, path: DbPath)
(implicit data: QueryData, ec: EC)
extends QueryBuilder[A]
{
  override def clone(implicit data: QueryData) =
    new RefQueryBuilder(targetIds, path)

  override def query = super.query ++ multiIdQuery(targetIds)
}

class EmbedQueryBuilder[A <: ModelT: BSONDocTypeMapper](
  name: String, path: DefinedPath)
(implicit data: QueryData, ec: EC, reader: BSONDocumentReader[A])
extends QueryBuilder[A]
{
  import commands.bson.BSONAggregationFramework._

  override def clone(implicit data: QueryData) =
    new EmbedQueryBuilder(name, path)

  override def query = idQuery(path.id)

  def embedQuery = super.query

  override def projection =
    BSONDocument(name -> mq"{ $$elemMatch: $embedQuery }")

  override def one() = {
    extractEmbedded(singleExtractor) map(_.headOption)
  }

  override def filter() = extractEmbedded(multiExtractor())

  override def ids() = {
    val idEx = Project(prefixedQuery(idQuery(1L), fullPath))
    fetchEmbedded(multiExtractor(IList(idEx)).widen[PipelineOperator])
      .map(extractIds)
  }

  def extractEmbedded(extractor: IList[PipelineOperator]) = {
    fetchEmbedded(extractor) map(buildModel)
  }

  def fetchEmbedded(extractor: IList[PipelineOperator]) = {
    collection.aggregate(Match(query), extractor.toList)
      .map { _.documents.headOption flatMap(_.get(name)) }
  }

  def buildModel(v: Option[BSONValue]) = {
    v collect {
      case BSONArray(docs) => docs collect {
        case scala.util.Success(a @ BSONDocument(_)) => reader.read(a)
      } toList
    } getOrElse(Nil)
  }

  def prefixedQuery(q: BSONDocument, prefix: String) = {
    if (prefix.isEmpty) q
    else BSONDocument(q.elements map { case (k, v) => s"$prefix.$k" -> v })
  }

  def singleExtractor = {
    multiExtractor(IList(Limit(1)).widen[PipelineOperator])
  }

  def pum(prefix: String, query: BSONDocument) = {
    (Project(BSONDocument(prefix -> 1, "_id" -> 0)) ::
      Unwind(prefix) ::
      Match(prefixedQuery(query, prefix)) ::
      Nil)
        .toIList.widen[PipelineOperator]
  }

  lazy val embedders = path match {
    case EmbeddedPath(_, _, embed) => embed.toIList.widen[PathComponent]
    case _ => INil[PathComponent]()
  }

  lazy val nestedPaths = {
    val paths = embedders.map(_.path).toIList <::: name.wrapNel
    paths.tail.foldLeft(paths.head.wrapNel) { (acc, path) =>
      acc append s"${acc.last}.$path".wrapNel
    }
  }

  lazy val nestedQueries = {
    embedders.map(e => idQuery(e.id)) <::: embedQuery.wrapNel
  }

  def liftEmbedded = {
    nestedPaths.zip(nestedQueries)
      .map { case (path, query) => pum(path, query) }
      .suml
  }

  def fullPath = nestedPaths.last

  // aggregation pipeline for extracting a single embedded attr list
  // 1. $project only the embedded attr, so each document contains only a list
  // of target docs
  // 2. $unwind to flatten the list of lists of targets
  // 3. $match to filter by the supplied query document
  // 4. apply any extra arguments
  // 5. $group to create a single array containing all target docs
  def multiExtractor(extra: IList[PipelineOperator] = INil()) = {
    liftEmbedded :::
    extra :::
    Group(idQuery("0"))(name -> Push(fullPath)) ::
    INil[PipelineOperator]()
  }

  val findIdJs = {
    s"""
    var findId = function(arr, id) {
      for(i = 0; i < arr.length; i++) {
        if(arr[i]._id.equals(id)) return parseInt(i, 10);
      }
      return -1;
    }
    """
  }

  def arrayIndices(path: IList[PathComponent]) = {
    def extractIndices(doc: BSONDocument) = {
      doc.getAs[BSONArray]("results") flatMap(_.values.headOption) match {
        case Some(d @ BSONDocument(_)) =>
          d.getAs[BSONArray]("value") map {
            _.values.toList collect {
              case BSONDouble(n) => n.toInt
              case BSONInteger(n) => n
            }
          }
        case _ => None
      }
    }
    def findIds(rest: IList[PathComponent], prefix: String): String = {
      rest match {
        case ICons(head, tail) =>
          val index = s"index${rest.length}"
          val attr = s"$prefix.${head.path}"
          val id = head.id.stringify
          val sub = findIds(tail, s"$attr[$index]")
          s"""
          var $index = findId($attr, ObjectId('$id'));
          if($index >= 0) {
            result.push($index);
            $sub
          }
          """
        case INil() => ""
      }
    }
    val ids = findIds(path, "this")
    val mapJs = {
      s"""
      function() {
        $findIdJs
        var result = [];
        $ids
        emit('index', result);
      }
      """
    }
    var reduceJs = "function(key, values) { return { 'index': values } }"
    val cmd = MapReduce(mapJs, reduceJs, Some(query))
    collection.runCommand(cmd) map(extractIndices) flatMap {
      _ some(ScalaFuture.successful) none {
        failFuture(s"Couldn't determine array indices for $embedders/$name")
      }
    }
  }

  def concreteIndexPath(indices: List[Int], path: List[PathComponent]) = {
    path
      .zip(indices)
      .map { case (e, i) => s"${e.path}.$i" }
      .mkString(".")
  }

  def addToSetByPath(data: A, path: String) = {
    val add = prefixedQuery(BSONDocument(name -> data.toBSONDoc), path)
    val update = mq"""{ $$addToSet : $add }"""
    collection.update(query, update)
  }

  def setByPath(data: BSONDocument, path: String) = {
    val set = prefixedQuery(data, path)
    val update = mq"{ $$set : $set }"
    collection.update(query, update)
  }

  import commands.WriteResult

  def modify(path: IList[PathComponent])(
    updater: String => ScalaFuture[WriteResult]) = {
    for {
      indices <- arrayIndices(path)
      writeResult <- updater(concreteIndexPath(indices, path.toList))
    } yield convertWriteResult(writeResult)
  }

  override def create(data: A) = {
    modify(embedders) { path => addToSetByPath(data, path) }
  }

  def updateById(id: ObjectId, data: BSONDocument) = {
    modify(embedders :+ SimplePathComponent(name, id)) { path =>
      setByPath(data, path)
    }
  }

  def convertWriteResult(result: WriteResult) = {
    if (result.ok) Result.ok
    else Result.err(result.message)
  }
}

trait CollectionDao[M <: ModelT]
extends DaoBase[M]
with QueryHelpers
{
  implicit def bsonDocMapper: BSONDocTypeMapper[M]

  protected val collection: BSONCollection

  def query(params: BSONDocument = BSONDocument())(implicit ec: EC) = {
    implicit val data = QueryData(
      collection,
      params,
      BSONDocument()
    )
    new QueryBuilder[M]
  }

  def q(implicit ec: EC) = query()

  def findById(id: ObjectId)(implicit ec: EC) = q.byId(id).one()

  override protected def allImpl(implicit ec: EC) = q.filter()

  def count()(implicit ec: EC): ScalaFuture[Int] =
    collection.runCommand(Count(emptyQuery)) map(_.value)

  def count[T](query: T)
  (implicit writer: BSONDocumentWriter[T], ec: EC): ScalaFuture[Int] =
    collection.runCommand(Count(writer.write(query))) map(_.value)

  protected def uncheckedRemoveById(id: ObjectId)(implicit ec: EC) =
    collection.uncheckedRemove(idQuery(id))

  protected def checkedRemoveById(id: ObjectId)(implicit ec: EC) =
    collection.remove(idQuery(id))

  def deleteWhere(query: BSONDocument)(implicit ec: EC) =
    collection.remove(query)

  def insertRaw(obj: BSONDocument)(implicit ec: EC) =
    collection.insert(obj)

  def updateWhere(params: BSONDocument, fields: BSONDocument)
  (implicit ec: EC) = {
    query(params).updateRaw(fields)
  }

  def updateById(id: ObjectId, fields: BSONDocument)
  (implicit ec: EC): ScalaFuture[M] = {
    collection
      .update(BSONDocument("_id" -> id.toBSON), fields)
      .flatMap { lastError =>
      if (lastError.inError)
        ScalaFuture.failed(new Throwable(lastError.message))
      else
        findById(id) flatMap {
          case Some(found) => ScalaFuture.successful(found)
          case None =>
            ScalaFuture.failed(
              new Exception(s"Could not re-find entity with id $id"))
        }
    }
  }

  protected def doInsert(m: M)(implicit ec: EC): ScalaFuture[M] =
    collection.insert(m) flatMap { lastError =>
      if (lastError.ok)
        ScalaFuture.successful(m)
      else
        ScalaFuture.failed(new Throwable(lastError.message))
    }

  protected def doUpdate(m: M)(implicit ec: EC): ScalaFuture[M] = {
    collection.update(
      selector = idQuery(m.id),
      update = m,
      upsert = true,
      multi = false) flatMap { lastError =>
        if (lastError.ok)
          ScalaFuture.successful(m)
        else
        ScalaFuture.failed(new Throwable(lastError.message))
      }
  }

  def validateInsert(data: M): ScalaFuture[ValidationNel[String, M]] = {
    ScalaFuture.successful(data.successNel)
  }

  def validateUpdate(data: ModelUpdater[M])
  : ScalaFuture[ValidationNel[String, BSONDocument]] = {
    ScalaFuture.successful(data.bson.successNel)
  }
}

trait Dao[M <: Model[M]]
extends CollectionDao[M]
{
  val name: String

  val mongoDb: MongoDb

  lazy val collection = mongoDb(name)

  override protected def insertImpl(m: M)(implicit ec: EC) =
    doInsert(m)

  override protected def updateImpl(m: M)(implicit ec: EC) =
    doUpdate(m)

  def removeById(id: ObjectId)(implicit ec: EC) =
    checkedRemoveById(id)
}
