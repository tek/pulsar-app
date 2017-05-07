package tryp
package slick

import scalaz._, Scalaz._

import com.github.nscala_time.time.Imports._

trait Timestamps
{
  val created: DateTime
  val updated: DateTime
}

trait Model
extends ModelT
with Timestamps
{
  def path: String

  def compactString: String

  def verbose(implicit ec: EC, dbi: tryp.slick.DbInfo): String

  def adapter(name: String, record: Boolean = true)
  : Option[AssocAdapterI]

  def age = (updated to DateTime.now).toDuration.getStandardDays
}

object Model
{
  implicit def showModel[A <: Model] = new Show[A]
  {
    override def show(m: A) = s"${m.className}(${m.id.stringify})"
  }
}

trait NamedModel
extends Model
{
  def name: String
}

object NamedModel
{
  implicit def showNamedModel[A <: NamedModel] = new Show[A]
  {
    override def show(m: A) = s"${m.className}(${m.name})"
  }
}
