package tryp.slick

import scalaz._, Scalaz._

import tryp.app._

@ModelsImpl(SyncSlickTransformer)
trait MobileSlick
extends SyncSlick
{
  class MobileModelOps(m: ModelSpec)
  extends SyncModelOps(m)
  {
    override def queryPath = (m.path getOrElse(super.queryPath)).toLowerCase
  }

  override def modelOps(cls: ModelSpec) = new MobileModelOps(cls)

  override def apply(spec: ModelsSpec) = {
    val models = spec.mobileModels
    spec.mergeTree(handle(models, spec.enums))
  }
}
