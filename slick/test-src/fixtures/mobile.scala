package tryp.slick.test.fixtures

import concurrent.ExecutionContext.Implicits.global

import tryp.annotation.MobileSlick

@MobileSlick()
object MobileTestSchema
{
  case class Mu(m: String)

  case class Iota(i: String, kap: Embed[List[Iota.Kappa]],
    mue: Embed[List[Mu]])
  {
    case class Kappa(k: String)
  }

  @mobile
  case class Eta(e: String, io: Embed[List[Iota]], th: Embed[List[Eta.Theta]])
  {
    case class Theta(t: String, lam: Embed[List[Eta.Theta.Lambda]])
    {
      case class Lambda(l: String)
    }
  }

  @forceMobile
  case class Nu(n: String, mau: List[Mu])
}

object asdfghjklmnopq
{
  // foo = -13
}
