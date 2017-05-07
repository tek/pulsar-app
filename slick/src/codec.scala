package tryp
package slick

import reflect.classTag

import com.github.nscala_time.time.Imports._

import io.circe._

object Codec
{

  implicit val encodeObjectId: Encoder[ObjectId] =
    Encoder.encodeString.contramap[ObjectId](i => i.stringify)

  implicit val decodeObjectId: Decoder[ObjectId] =
    Decoder.decodeString.emap { str =>
      Xor.catchNonFatal(ObjectId(str)).leftMap(t => "ObjectId")
    }
}

// object Codec
// {
//   def fail[A: ClassTag](error: String, c: HCursor) = {
//     val what = classTag[A].className
//     DecodeResult.fail[A](s"Error parsing json for $what: $error", c.history)
//   }

//   def decoder[A: ClassTag, B](convert: String => B)(construct: B => A) = {
//     c: HCursor =>
//       c.focus.string match {
//         case Some(s) =>
//           Try(convert(s)) match {
//             case util.Success(r) => DecodeResult.ok(construct(r))
//             case util.Failure(e) => fail[A](e.toString, c)
//           }
//         case None => fail[A](s"Invalid input '${c.focus}'", c)
//       }
//   }

//   val dateTimeDecoder = decoder[DateTime, Long](_.toLong)(_.toDateTime)

//   implicit val dateTimeCodecJson: CodecJson[DateTime] = CodecJson(
//     (dt: DateTime) => jString(dt.unix.toString), dateTimeDecoder
//   )

//   val objectIdDecoder = decoder[ObjectId, String](identity)(ObjectId(_))

//   implicit val objectIdCodecJson: CodecJson[ObjectId] = CodecJson(
//     (id: ObjectId) => jString(id.stringify), objectIdDecoder
//   )

//   def enumDecoder(enum: Enumeration) = {
//     decoder[enum.Value, String](identity)(enum.withName(_))
//   }

//   def enumCodecJson(enum: Enumeration) = CodecJson(
//     (v: enum.Value) => jString(v.toString), enumDecoder(enum)
//   )
// }
