package tryp
package slick

@exportNames(DbInfo)
trait Exports
{
  val DroidDbInfo = tryp.slick.DroidDbInfo
  type Model = tryp.slick.Model
  type NamedModel = tryp.slick.NamedModel
}

trait All
extends ModelQueryExt
with ActionExt
with TypedType
{
  import shapeless._
  import com.github.nscala_time.time.Imports.DateTime

  type MNil = ObjectId :: DateTime :: DateTime :: HNil
  lazy val MNil = ObjectId.generate :: DateTime.now :: DateTime.now :: HNil

  import _root_.slick.lifted._
  import _root_.slick.profile._
  import _root_.slick.driver._
  import _root_.slick.dbio._

  type DbAction = AnyAction[_]
  type AnyModelTable = ModelTableI[_] {
    type TableElementType <: Model
  }
  type ModelTableQuery[A <: Model] = TableQuery[_ <: ModelTable[A]]
  type Dao[A <: Model] = tryp.slick.TrypQuery[A, _ <: ModelTable[A]]
    with ModelTableQuery[A]
  type AnyAction[A] = DBIOAction[A, NoStream, Effect.All]
  type AssocIds = (ObjectId, ObjectId)
  type ModelT = tryp.app.ModelT
  type ModelTable[A <: Model] = ModelTableI[A] {
    type TableElementType = A
  }
}

@integrate(app)
object `package`
extends All
{
}
