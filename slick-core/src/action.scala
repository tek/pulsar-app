package tryp
package slick

import concurrent.Await

import scalaz._, Scalaz._, concurrent._, stream._
import OptionT._

import _root_.slick.driver.SQLiteDriver.api._
import _root_.slick.dbio.DBIO

// import tryp.slick.DbInfo
// import SlickTypes._

trait ActionExt
{
  implicit def dbioInstance(implicit ec: EC) = new scalaz.Monad[AnyAction]
  {
    def point[A](a: => A): AnyAction[A] = DBIO.successful(a)

    def bind[A, B](fa: AnyAction[A])(f: A => AnyAction[B]): AnyAction[B] =
      fa flatMap f
  }

  implicit class ToDBIOOps[A](value: A)
  {
    def toDbio = toDbioSuccess

    def toDbioSuccess = DBIO.successful(value)

    def wrapDbioSeq: AnyAction[Seq[A]] = toDbio.wrapDbioSeq
  }

  implicit class StringToDBIOOps(s: String)
  {
    def toDbioFail = DBIO.failed(new Throwable(s))
  }

  implicit class FutureToDBIOOps[A](fut: ScalaFuture[A])
  {
    def toDbio = DBIO.from(fut)
  }

  implicit class TryToDBIOOps[A](value: Try[A])
  {
    def toDbio = ScalaFuture.fromTry(value).toDbio
  }

  implicit class OptionToDBIOOps[A](value: Option[A])
  {
    def toDbio(error: String) = {
      value some(DBIO.successful) none(error.toDbioFail)
    }
  }

  implicit class OptionActionOps[R](action: AnyAction[Option[R]])
  (implicit ec: EC)
  {
    def orElse(that: => AnyAction[Option[R]]) = {
      action.flatMap(_ some(_ => action) none(that))
    }

    def getOrElse(that: => AnyAction[R]) = {
      action.flatMap(_ some[AnyAction[R]](DBIO.successful) none(that))
    }

    def flatten = {
      getOrElse(DBIO.failed(new Throwable("flatten")))
    }

    def t = optionT[AnyAction](action)

    def fold[A](error: => AnyAction[A])(success: R => AnyAction[A]) = {
      action.flatMap(_ some(success) none(error))
    }

    def foldM[A](error: => String)(success: R => AnyAction[A]) = {
      fold(error.toDbioFail)(success)
    }
  }

  implicit class OptionalActionOps[A[_]: Optional, B](
    action: AnyAction[A[B]])
  (implicit ec: EC)
  {
    def nelM(msg: String) = {
      action map { r =>
        r.pextract match {
          case -\/(_) => msg.failureNel[B]
          case \/-(v) => v.successNel[String]
        }
      }
    }

    def nel(implicit ct: ClassTag[B]) = {
      nelM(s"empty Optional[${className[B]}]")
    }
  }

  implicit class ValidationNelActionOps[E, R]
  (action: AnyAction[ValidationNel[E, R]])
  (implicit ec: EC)
  {
    def vmap[R2](f: R => R2): AnyAction[ValidationNel[E, R2]] = {
      action map(_.map(f))
    }

    def vflatMap[R2]
    (f: R => AnyAction[R2]): AnyAction[ValidationNel[E, R2]] = {
      action flatMap {
        _ match {
          case scalaz.Success(r) => f(r) map(_.successNel)
          case e @ scalaz.Failure(_) => e.toDbio
        }
      }
    }

    def nelFlatMap[R2]
    (f: R => AnyAction[ValidationNel[E, R2]]): AnyAction[ValidationNel[E, R2]] =
    {
      action flatMap {
        _ match {
          case scalaz.Success(r) => f(r)
          case e @ scalaz.Failure(_) => e.toDbio
        }
      }
    }

    def <*>[R2](next: AnyAction[ValidationNel[E, R => R2]]) = {
      action flatMap(v1 => next map(v1 <*> _))
    }

    def *>[R2](next: AnyAction[ValidationNel[E, R2]]) = {
      action flatMap(v1 => next map(v1 *> _))
    }

    def orElse[R2 >: R]
    (g: => AnyAction[ValidationNel[E, R2]]) = {
      action flatMap { a =>
        a match {
          case scalaz.Success(_) => a.toDbio
          case f @ scalaz.Failure(_) => (f.toDbio *> g)
        }
      }
    }
  }

  implicit class ActionTraversableOps[R](action: Traversable[AnyAction[R]])
  {
    def toDbioSeq = DBIO.sequence(action)
    def toDbio = toDbioSeq
  }

  implicit class ActionOps[R](action: AnyAction[R])
  {
    def wrapDbioSeq = DBIO.sequence(Seq(action))

    def vmap[R2](f: R => R2)
    (implicit ec: EC): AnyAction[ValidationNel[String, R2]] = {
      action map(f >>> { _.successNel })
    }
  }

  implicit class ActionEffectOps[+R, +S <: NoStream, -E <: Effect](
    a: DBIOAction[R, S, E])
  (implicit info: DbInfo,  ec: EC)
  {
    def exec() = info.db() run(a)

    // FIXME does this make any sense?
    def execAndClose() = {
      exec() andThen { case _ => info.db().close() }
    }

    // FIXME move to test?
    def get(timeout: Duration = Duration.Inf) = {
      Await.result(exec(), timeout)
    }

    def !() = exec()

    def !!() = get()

    def unsafePerformSyncAttempt = a.asTry map { _ match {
      case util.Success(a) => \/-(a)
      case util.Failure(t) => -\/(t)
    } }

    def effectIfFailed(effect: Throwable => Unit) = {
      a.cleanUp { e =>
        e.foreach(effect)
        a
      }
    }

    def recover[U >: R](pf: PartialFunction[Throwable, U]) = {
      a.asTry flatMap { _ recover(pf) toDbio }
    }

    def recoverWith[U >: R](pf: PartialFunction[Throwable, AnyAction[U]]) = {
      a.asTry flatMap { t =>
        t match {
          case util.Success(res) => res.toDbio
          case util.Failure(err) =>
            pf.lift(err) getOrElse(DBIO.failed(err))
        }
      }
    }

    def tap(e: R => Any) = {
      a map { r =>
        e(r)
        r
      }
    }

    def futureMap[R2](f: R => ScalaFuture[R2]) = a flatMap(f >>> DBIO.from _)

    def task = Task(!!)

    def proc = Process.eval(task)
  }

  implicit def TraversableActionMonoid[A](implicit ec: EC) =
    new Monoid[AnyAction[Traversable[A]]]
  {
    def zero = DBIO.sequence(List())
    def append(a: AnyAction[Traversable[A]], b: => AnyAction[Traversable[A]]) =
    {
      for {
        as <- a
        bs <- b
      } yield as ++ bs
    }
  }

  def suspendedDbio[A](act: => AnyAction[A])(implicit ec: EC) =
    nopDbio andThen(act)

  def suspendedDbioLift[A](act: => A)(implicit ec: EC) =
    nopDbio map(_ => act)

  def nopDbio = ().toDbio
}

object ActionExt
extends ActionExt
