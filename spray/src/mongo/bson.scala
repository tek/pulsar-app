package tryp
package mongo

import concurrent.ExecutionContext
import collection.generic.CanBuildFrom

import com.github.nscala_time.time.Imports._

import scalaz._, Scalaz._

import reactivemongo._
import core.commands.{Command, Count, LastError}
import core.commands.Count
import core.commands.LastError

import play.api.libs.iteratee.Iteratee

import api.collections.bson.BSONCollection
import bson._
import DefaultBSONHandlers._

import _root_.spray.json._
import DefaultJsonProtocol._

trait BsonTypes
{
  type BVWriter[A] = BSONWriter[A, BSONValue]
  type BVReader[A] = BSONReader[BSONValue, A]
}

object BsonTypes
extends BsonTypes

import BsonTypes._

trait BSONMapper
{
  def fail(v: BSONValue) =
    sys.error(s"Unexpected BSON value for deserialization: $v")
}

trait BSONTypeMapper[A]
extends BVWriter[A]
with BVReader[A]
with BSONMapper

trait BSONDocTypeMapper[A]
extends BVReader[A]
with BSONDocumentWriter[A]
with BSONMapper
with NormalizedIdTransformer
{
  def read(v: BSONValue) = v match {
    case d @ BSONDocument(_) => readDoc(d)
    case _ => fail(v)
  }

  def readDoc(d: BSONDocument): A
}

trait BSONDocTypeWriter[A]
extends BSONDocumentWriter[A]
with NormalizedIdTransformer

trait BsonProtocol
extends DefaultBSONHandlers
{
  implicit object BSONValueTypeMapper
  extends BSONTypeMapper[BSONValue]
  {
    def write(s: BSONValue) = s
    def read(v: BSONValue) = v
  }

  implicit object StringTypeMapper
  extends BSONTypeMapper[String]
  {
    def write(s: String) = BSONString(s)
    def read(v: BSONValue) = v match {
      case BSONString(str) => str
      case v => fail(v)
    }
  }

  implicit object LongTypeMapper
  extends BSONTypeMapper[Long]
  {
    def write(l: Long) = BSONLong(l)
    def read(v: BSONValue) = v match {
      case BSONLong(l) => l
      case v => fail(v)
    }
  }

  implicit object DoubleTypeMapper
  extends BSONTypeMapper[Double]
  {
    def write(l: Double) = BSONDouble(l)
    def read(v: BSONValue) = v match {
      case BSONDouble(l) => l
      case v => fail(v)
    }
  }

  implicit object ObjectIdTypeMapper
  extends BSONTypeMapper[ObjectId]
  {
    def write(i: ObjectId) = i
    def read(v: BSONValue) = v match {
      case i: ObjectId => i
      case _ => fail(v)
    }
  }

  implicit object DateTimeTypeMapper
  extends BSONTypeMapper[DateTime]
  {
    def write(dt: DateTime) = BSONLong(dt.unix)
    def read(v: BSONValue) = v match {
      case BSONLong(l) => Try(l.toDateTime) match {
        case scala.util.Success(dt) => dt
        case _ => fail(v)
      }
      case _ => fail(v)
    }
  }

  def enumTypeMapper[A <: Enumeration](enum: A) = new BSONTypeMapper[A#Value] {
    def write(v: A#Value) = BSONString(v.toString)

    def read(v: BSONValue) = {
      v match {
        case BSONString(s) => Try(enum.withName(s)) match {
          case scala.util.Success(e) => e
          case _ => fail(v)
        }
          case _ => fail(v)
      }
    }
  }

  implicit object EmptyRecordTypeMapper
  extends BSONDocTypeMapper[EmptyRecord]
  {
    def write(r: EmptyRecord) =
      BSONDocument(transformForBSON("id") -> r.id.toBSON)

    def readDoc(v: BSONDocument) =
      v.getAs[ObjectId](transformForBSON("id")) match {
        case Some(id) => EmptyRecord(id)
        case None => fail(v)
      }
  }

  implicit def traversableMapper[A, B[A] <: Traversable[A]]
  (implicit elMapper: BSONTypeMapper[A], cbf: CanBuildFrom[Nothing, A, B[A]]) =
    new BSONTypeMapper[B[A]] {
    def write(t: B[A]) = BSONArray(t map(_.toBSON))
    def read(v: BSONValue) = {
      v match {
        case BSONArray(elements) => {
          val s: Stream[A] = elements collect {
            case scala.util.Success(e) => elMapper.read(e)
            case scala.util.Failure(error) => fail(v)
          }
          s.to[B]
        }
        case _ => fail(v)
      }
    }
  }

  implicit def traversableDocMapper[A, B[A] <: Traversable[A]]
  (implicit m: BSONDocTypeMapper[A], cbf: CanBuildFrom[Nothing, A, B[A]]) = {
    implicit val conv = docMapperToMapper(m)
    traversableMapper[A, B]
  }

  implicit def mapMapper[A: BSONTypeMapper]: BSONTypeMapper[Map[String, A]] =
    new BSONTypeMapper[Map[String, A]]
    {
      def write(m: Map[String, A]) = mapDocMapper[A].write(m)

      def read(v: BSONValue) = {
        val strings = v match {
          case b: BSONDocument => mapDocMapper[A].read(b)
          case _ => fail(v)
        }
        strings.toMap
      }
    }

  implicit def mapDocMapper[A](implicit amapper: BSONTypeMapper[A]) =
    new BSONDocTypeMapper[Map[String, A]]
    {
      def write(m: Map[String, A]) = BSONDocument(
        m.map { case (k, v) => transformForBSON(k) -> v.toBSON }
      )

      def readDoc(v: BSONDocument) = {
        val strings = v.elements collect {
          case (k, v) => transformFromBSON(k) -> amapper.read(v)
          case _ => fail(v)
        }
        strings.toMap
      }
    }

  implicit class BsonValWrite[A](a: A)
  (implicit mapper: BVWriter[A])
  {
    def toBSON: BSONValue = mapper.write(a)
    def toBSONVal: BSONValue = toBSON
  }

  implicit class BsonValRead[A](v: BSONValue)
  (implicit mapper: BSONReader[BSONValue, A])
  {
    def fromBSON: A = mapper.read(v)
  }

  implicit class BsonDocWrite[A](a: A)
  (implicit writer: BSONDocumentWriter[A])
  {
    def toBSONDoc: BSONDocument = writer.write(a)
  }

  implicit class BsonValWriteDoc[A: BSONDocumentWriter](a: A)
  {
    def toBSON: BSONValue = a.toBSONDoc
  }

  implicit object BsonDocBsonValWriter
  extends BVWriter[BSONDocument]
  {
    def write(doc: BSONDocument) = doc
  }

  import reactivemongo.api.commands.ResolvedCollectionCommand

  implicit object MapReduceWriter
      extends BSONDocumentWriter[ResolvedCollectionCommand[MapReduce]] {
    def write(mapr: ResolvedCollectionCommand[MapReduce]): BSONDocument = {
      val cmd = mapr.command
      BSONDocument(
        "mapReduce" -> BSONString(mapr.collection),
        "map" -> BSONString(cmd.mapFunction),
        "reduce" -> BSONString(cmd.reduceFunction),
        "out" -> BSONDocument("inline" -> true),
        "query" -> cmd.query
      )
    }
  }

  implicit def docMapperToDocReader[A](implicit bv: BSONDocTypeMapper[A]) =
    new BSONDocumentReader[A] {
      def read(d: BSONDocument) = bv.read(d)
    }

  def docMapperToMapper[A](implicit m: BSONDocTypeMapper[A]) =
    new BSONTypeMapper[A] {
      def write(a: A) = m.write(a)
      def read(v: BSONValue) = m.read(v)
    }

  implicit object BSONValueShow
  extends Show[BSONValue]
  {
    override def show(b: BSONValue) = {
      b match {
        case d @ BSONDocument(_) => BSONDocumentShow.show(d)
        case a @ BSONArray(_) => BSONArrayShow.show(a)
        case v => v.toString
      }
    }
  }

  implicit object BSONDocumentShow
  extends Show[BSONDocument]
  {
    override def show(doc: BSONDocument) = {
      val sub = doc.elements
        .map { case (k, v) => s"$k: ${v.show}" }
        .mkString(", ")
      s"{ $sub }"
    }
  }

  implicit object BSONValueListShow
  extends Show[List[BSONValue]]
  {
    override def show(l: List[BSONValue]) = {
      l map(_.show) mkString(", ")
    }
  }

  implicit object BSONArrayShow
  extends Show[BSONArray]
  {
    override def show(b: BSONArray) = {
      val sub = b.values.toList.show
      s"[$sub]"
    }
  }

  import api.commands.bson.BSONAggregationFramework.PipelineOperator

  implicit object PipelineOperatorShow
  extends Show[PipelineOperator]
  {
    override def show(op: PipelineOperator) = {
      op.makePipe match {
        case d: BSONDocument => d.show
        case a => a.toString
      }
    }
  }

  implicit object AggOperatorShow

  implicit object AggOperatorList
  extends Show[List[PipelineOperator]]
  {
    override def show(l: List[PipelineOperator]) = l map(_.show) mkString(", ")
  }
}

object BsonProtocol
extends BsonProtocol

trait ElementNameTransformer {
  def transformForBSON(name: String): String = name
  def transformFromBSON(name: String): String = name
}

trait IdElementNameTransformer
extends ElementNameTransformer
{
  def idFieldName: String

  abstract override def transformForBSON(name: String): String =
    if (super.transformForBSON(name) == idFieldName) "_id"
    else super.transformForBSON(name)
  abstract override def transformFromBSON(name: String): String =
    if (super.transformFromBSON(name) == "_id") idFieldName
    else super.transformFromBSON(name)
}

trait NormalizedIdTransformer
extends IdElementNameTransformer
{
  override def idFieldName = "id"
}
