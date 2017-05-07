package tryp
package slick

import com.github.nscala_time.time.Imports._

import _root_.slick.jdbc.SQLiteProfile.api._

trait TypedType
{
  def enum[A <: Enumeration](e: A) =
    MappedColumnType.base[e.Value, String](_.toString, s => e.withName(s))

  implicit val dateTime =
    MappedColumnType.base[DateTime, Long](
    { dt => dt.getMillis },
    { t => t.toDateTime }
  )

  implicit val objectId =
    MappedColumnType.base[ObjectId, String](
    { a => a.stringify },
    { a => ObjectId(a) }
  )
}

object TypedType
extends TypedType
