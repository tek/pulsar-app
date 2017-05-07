package tryp.slick.test

import concurrent.ExecutionContext.Implicits.global

import org.specs2._
import org.specs2.specification._

@tryp.annotation.Slick
object BasicTest
{
  case class Alpha(a: String)
}

object `compilation stopper`
{
  // foo = 0
}

object DB
{
  implicit val dbInfo = tryp.slick.TestFileDbInfo("basic")
  implicit val profile = dbInfo.profile
}

import DB._

object BasicInstance
extends BasicTest.Slick
