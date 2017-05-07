package tryp.slick.test.fixtures

import concurrent.ExecutionContext.Implicits.global

import tryp.annotation.SyncSlick

@SyncSlick()
object NestTestSchema
{
  object Nummy
  extends Enumeration
  {
    type Nummy = Value
    val Val = Value
  }

  case class Delta(d: String)
  case class Gamma(g: String, betas: Embed[List[Gamma.Beta]],
    alps: Embed[List[Gamma.Beta.Alpha]])
  {
    case class Beta(b: String, alps: Embed[List[Gamma.Beta.Alpha]])
    {
      case class Alpha(a: String, deltas: Embed[List[Delta]])
    }
  }
  // foo = -13
}
