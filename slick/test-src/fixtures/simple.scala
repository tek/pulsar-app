package tryp.slick.test.fixtures

import concurrent.ExecutionContext.Implicits.global

import tryp.annotation.Slick

@Slick()
object SimpleTestSchema
{
  object Flag extends Enumeration {
    val On = Value
    val Off = Value
  }

  case class Alpha(name: String, flagger: Flag.Value)
  case class Beta(name: String, alp: Alpha)
  case class Gamma(name: String, bets: List[Beta])
}
