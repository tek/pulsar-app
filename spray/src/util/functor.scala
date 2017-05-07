package tryp.spray.meta

import scalaz._, Scalaz._

import tryp.mongo.Model

final class ModelFunctorOps
[A <: Model[A], B[_]: Functor]
(a: B[A])
{
  def ids = a map(_.id)
}

trait ToModelFunctorOps
{
  implicit def ToModelFunctorOps[A <: Model[A], B[_]: Functor](a: B[A]) = {
    new ModelFunctorOps(a)
  }
}
