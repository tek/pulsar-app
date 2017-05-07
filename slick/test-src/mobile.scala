package tryp.slick.test

import scala.concurrent.Await

import scalaz._, Scalaz._

import tryp.slick._
import tryp.slick._
import tryp.slick.sync._
import tryp.slick.meta.QueryExt._
import Codec._
import fixtures.MobileTestSchema

abstract class MobileSchemaTest
extends SchemaSpecBase
{ spec =>
  import profile.api._

  object Schema
  extends MobileTestSchema.Slick
}

class MobileSchemaFetchSpec
extends MobileSchemaTest
{
  def is = s2"""
  local replication of subtrees for mobile apps

  use the attribute name as path $attrPath
  """

  import Schema.Exports._
  import scala.concurrent.ExecutionContext.Implicits.global

  def beforeAll() {
    resetDb()
    sync()
  }

  def attrPath = {
    rest.requests.map(_._1).toSeq must_== List("/nu", "/io", "/th")
  }.pendingUntilFixed("exclude top level models that are only embedded")
}
