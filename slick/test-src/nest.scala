package tryp.slick.test

import scala.concurrent.Await

import scalaz._, Scalaz._

import tryp.slick._
import tryp.slick._
import tryp.slick.sync._
import tryp.slick.meta.QueryExt._
import Codec._
import fixtures.NestTestSchema
import SyncResult._

abstract class NestSchemaTest
extends SchemaSpecBase
{ spec =>
  import profile.api._

  object Schema
  extends NestTestSchema.Slick
}

class NestSchemaFetchSpec
extends NestSchemaTest
{
  def is = s2"""
  Nested data

  fetch nested data $fetch
  apply correct associations $assoc
  """

  import profile.api._
  import Schema.Exports._
  import scala.concurrent.ExecutionContext.Implicits.global

  val deltaName = "response_delta_1"

  override val responses = {
    val d1 = Delta.Mapper(deltaName, oid("d1"))
    val g1 = Gamma.Mapper("g", oid("g1"))
    val b1 = Gamma.Beta.Mapper("b", oid("b1"))
    val a1 = Gamma.Beta.Alpha.Mapper("a", oid("a1"))
    Seq(d1).js ::
      Seq(g1).js ::
      Seq(b1).js ::
      Seq(a1).js ::
      Seq(d1).js ::
      Seq(d1.id).js ::
      Seq(a1.id).js ::
      Seq(b1.id).js ::
      "[]" ::
      "[]" ::
      Nil
  }

  def beforeAll() {
    resetDb()
    sync()
  }

  def embeddedDelta = Gamma.Beta.Alpha.Deltas.one.get()

  def fetch = {
    embeddedDelta.map(_.d) must beSome(deltaName)
  }

  def assoc = {
    Gamma.one.get().get.betas
    Gamma.one.get().get.betas.ids.get()
    Gamma.one.get().get.betas.all.get()
    val d = for {
      g <- Gamma.one.get()
      b <- g.betas.one.get()
      a <- b.alps.one.get()
      d <- a.deltas.one.get()
    } yield d
    d must_== embeddedDelta
  }
}

class NestSchemaSendSpec
extends NestSchemaTest
{
  def is = s2"""
  Nested data

  send $send
  """

  import Schema.Exports._
  import scala.concurrent.ExecutionContext.Implicits.global

  def createModels = {
    val res = for {
      d <- Delta.create("delta", oid("d1"))
      a <- Gamma.Beta.Alps.create("alpha", oid("a1"))
      b <- Gamma.Betas.create("b", oid("b1"))
      c <- Gamma.create("g", oid("c1"))
    } yield (a, b, c, d)
    res.get() tap { case (a, b, c, d) =>
      a.deltas.add(d.id).get()
      b.alps.add(a.id).get()
      c.betas.add(b.id).get()
    }
  }

  lazy val models = createModels

  val d = Delta.Mapper("delta", oid("d1"))
  val c = Gamma.Mapper("g", oid("c1"))
  val b = Gamma.Beta.Mapper("b", oid("b1"))
  val a = Gamma.Beta.Alpha.Mapper("alpha", oid("a1"))

  override val responses = {
    d.js ::
      Seq(d).js ::
      c.js ::
      Seq(c).js ::
      b.js ::
      Seq(b).js ::
      a.js ::
      Seq(a).js ::
      Seq(d).js ::
      Seq(d.id).js ::
      Seq(a.id).js ::
      Seq(b.id).js ::
      "[]" ::
      "[]" ::
      Nil
  }

  def beforeAll() {
    resetDb()
    models
    sync()
  }


  def send = {
    rest.requests(7)._1 must_== s"/gamma/${c.id.s}/betas/${b.id.s}/alps"
  }
}
