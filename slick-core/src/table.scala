package tryp.slick

import slick.lifted._

trait ModelTableI[A <: Model]
extends AbstractTable[A]
{
  def id: Rep[ObjectId]
}
