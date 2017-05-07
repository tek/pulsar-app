package tryp.slick.test.fixtures

import concurrent.ExecutionContext.Implicits.global

import tryp.annotation.SyncSlick

@SyncSlick()
object ExtTestSchema
{
  object Flag
  extends Enumeration
  {
    val On = Value
    val Off = Value
  }

  case class Alpha(name: String, flog: Flag.Value, num: Double)
  case class Beta(name: String, killed: Option[DateTime], alp: Alpha,
    alp2: Alpha)
  case class Gamma(name: String, bets: List[Beta], bet2s: List[Beta],
    alps: Embed[List[Alpha]])
}

object asdfghjk
{
  // foo = -1
}
