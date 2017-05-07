package tryp.slick

import org.specs2._
import org.specs2.specification._

@MetaTestSchema()
object MetaTest
{
  object Flag extends Enumeration {
    type Flag = Value
    val On = Value
    val Off = Value
  }

  case class Alpha(name: String, flog: Flag.Value)
  case class Beta(name: String, alp: Alpha)
  case class Gamma(bets: List[Beta], opt: Option[Alpha], alp: Alpha,
    style: Int)
}

class MetaTest
extends MetaTest.MetaTest
{
  // foo = -2
}
