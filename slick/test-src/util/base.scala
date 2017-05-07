package tryp.slick.test

import org.specs2._
import org.specs2.specification._

abstract class SlickSpecBase
extends Specification
with BeforeAll
with tryp.slick.SlickTestDb
with tryp.TrivialImplTestHelpers
