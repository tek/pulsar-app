package tryp
package spray

import _root_.spray.httpx.unmarshalling.FromRequestUnmarshaller
import _root_.spray.httpx.marshalling.ToResponseMarshaller
import _root_.spray.json._

import com.github.nscala_time.time.Imports._

case class JsonParseError(target: String, input: JsValue)
extends java.lang.RuntimeException(s"Invalid json input for $target: $input")

trait ModelCreator[A]
{
  def model: A
}

trait ModelUpdater[A]
{
  def bson: reactivemongo.bson.BSONDocument
  def update(other: A): A
}

trait Marshalling[A <: Model[A], B <: ModelCreator[A], C <: ModelUpdater[A]]
{
  implicit val post: FromRequestUnmarshaller[B]
  implicit val put: FromRequestUnmarshaller[C]
  implicit val toResponse: ToResponseMarshaller[A]
}

trait BasicJson
extends DefaultJsonProtocol
{
  def error(name: String, v: JsValue) = throw JsonParseError(name, v)

  implicit val `ObjectId format` = new JsonFormat[ObjectId] {
    def write(o: ObjectId) = JsString(o.stringify)

    def read(value: JsValue) = {
      value match {
        case JsString(id) => ObjectId(id)
        case a => error("ObjectId", a)
      }
    }
  }

  implicit val `DateTime format` = new JsonFormat[DateTime] {
    def write(dt: DateTime) = JsString(dt.unix.toString)

    def read(value: JsValue) = {
      value match {
        case j @ JsString(s) =>
          Try(s.toLong.toDateTime) match {
            case Success(dt) => dt
            case Failure(_) => error("DateTime", j)
          }
        case JsNumber(n) if n.isValidLong => n.toLong.toDateTime
        case a => error("DateTime", a)
      }
    }
  }

  def jsonEnum[T <: Enumeration](enu: T) = new JsonFormat[T#Value] {
		def write(obj: T#Value) = JsString(obj.toString)

		def read(json: JsValue) = json match {
			case JsString(txt) => enu.withName(txt)
			case something =>
        throw new DeserializationException(
          s"Expected a value from enum $enu instead of $something")
		}
	}
}

object BasicJson
extends BasicJson
