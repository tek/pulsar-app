package tryp
package slick

import _root_.slick.lifted._
import _root_.slick.profile._
import _root_.slick.basic._
import _root_.slick.jdbc.JdbcProfile

trait QueryExt
extends ActionExt
{
  implicit class QueryOps[+E, U, C[_]](q: Query[E, U, C])
  (implicit val profile: BasicProfile)
  {
    import profile.api._
    import _root_.slick.dbio.Effect

    def ! = q.result

    // result gets type from profile.api imports, which is volatile.
    // import of Effect is necessary because it is referenced from profile
    // as well
    def one =
      (q.result: BasicStreamingAction[C[U], U, Effect.All]).headOption
  }

  implicit class QueryResultOps[+E, U, C[_]](q: Query[E, U, C])
  (implicit val profile: BasicProfile, ec: EC)
  {
    import profile.api._

    def flatOneOr(action: => AnyAction[Option[U]]) = {
      q.one.orElse(action)
    }

    def oneOr(action: => AnyAction[U]) = {
      q.one.getOrElse(action)
    }
  }

  implicit class QueryEffectOps[+E, U, C[_]](q: Query[E, U, C])
  (implicit val profile: BasicProfile, dbi: DbInfo, ec: EC)
  {
    import profile.api._

    def get(timeout: Duration = 10 seconds) = q.result.get(timeout)

    def !! = get()
  }
}

trait ModelQueryExt
extends QueryExt
{
  implicit class ModelQueryOps[+E <: ModelTable[U], U <: Model, C[_]]
  (q: Query[E, U, C])
  (implicit val profile: JdbcProfile)
  {
    import profile.api._

    def byId(id: ObjectId) = q filter(_.id === id) one
  }
}

object QueryExt
extends QueryExt

object ModelQueryExt
extends ModelQueryExt
