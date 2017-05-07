package tryp.app.meta

import scalaz._, Scalaz._

import tryp.app.{ModelT, ObjectId}

final class ModelTFunctorOps[F[_]: Functor, B <: ModelT](a: F[B])
{
  def idF: F[ObjectId] = a map(_.id)
}

trait ToModelTFunctorOps
{
  implicit def ToModelFunctorOps[F[_]: Functor, B <: ModelT](a: F[B]) =
    new ModelTFunctorOps(a)
}

final class ModelTFoldableOps[F[_]: Foldable: Functor, B <: ModelT](a: F[B])
extends ToModelTFunctorOps
{
  def idList: List[ObjectId] = a.idF.toList
  def ids: Set[ObjectId] = a.idF.toSet
}

trait ToModelTFoldableOps
{
  implicit def ToModelFoldableOps[F[_]: Foldable: Functor, B <: ModelT]
  (a: F[B]) =
    new ModelTFoldableOps(a)
}

final class ObjectIdOps(id: ObjectId)
{
  def s = id.stringify
}

trait ToObjectIdOps
{
  implicit def ToObjectIdOps(id: ObjectId) = new ObjectIdOps(id)
}
