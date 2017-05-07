package tryp.slick.test

import concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Await

import com.github.nscala_time.time.Imports.DateTime

import scalaz._, Scalaz._

import io.circe._

import org.specs2._
import org.specs2.specification._

import fixtures.ExtTestSchema

abstract class ExtSchemaTest
extends SchemaSpecBase
{ spec =>

  import profile.api._

  object Schema extends ExtTestSchema.Slick

  import Schema.Exports._

  def createModels = {
    for {
      a <- Alpha.create("alpha1", Flag.On, 4.0, oid("a1"))
      a2 <- Alpha.create("alpha2", Flag.On, 4.0, oid("a2"))
      b <- Beta.create("beta1", None, a.id, a2.id, oid("b1"))
      b2 <- Beta.create("beta2", Some(DateTime.now), a.id, a2.id,
        oid("b2"))
      c <- Gamma.create("gamma1", oid("c1"))
      c2 <- Gamma.create("gamma2", oid("c2"))
      a3 <- c2.alps.insert(Alpha("alpha3", Flag.Off, 1.0, oid("a3")))
    } yield (a, b, b.id, c, a3)
  }

  lazy val models = createModels.!!

  def alphas = Alpha.!!
  def betas = Beta.!!
  def gammas = Gamma.!!
}

class BasicExtSchemaTest
extends ExtSchemaTest
{
  def is = s2"""
  The extended schema should

  create models $checkModels
  add list associations $listAssoc
  add flat associations $flatAssoc
  create pending actions for alphas $pendingAlpha
  create pending actions for betas $pendingBeta
  create pending actions for gammas $pendingGamma
  create pending actions for embedded alpha $pendingEmbeddedAlpha
  """

  import Schema.Exports._

  def beforeAll() {
    resetDb()
  }

  def checkModels = Try(models) must beSuccessfulTry

  def listAssoc = {
    val (a, b, bId, c, a3) = models
    c.bets.add(bId).!!
    c.bet2s.add(bId).!!
    c.bets.all.!! must contain(b)
  }

  def flatAssoc = {
    val (a, b, bId, c, a3) = models
    b.alpQuery must_!= b.alp2Query
  }

  import Schema.pending.Addition

  def pendingAlpha = {
    additionTargets("alpha") === alphas.map(_.id)
  }

  def pendingBeta = {
    additionTargets("beta") === betas.map(_.id)
  }

  def pendingGamma = {
    additionTargets("gamma") === gammas.map(_.id)
  }

  def pendingEmbeddedAlpha = {
    val (a, b, bId, c, a3) = models
    additionTargets("gamma.alps") === List(a3.id)
  }
}

class CompletePendingTest
extends ExtSchemaTest
{
  def is = s2"""
  After completing the first alpha addition, there should be

  one alpha addition left $alpha
  both beta additions left $beta
  both gamma additions left $gamma
  """

  import Schema.Exports._
  import Schema.pending.Exports._

  def beforeAll() {
    resetDb()
    val (a, b, bId, c, a3) = models
    Alpha.completeSync(additions("alpha").head).!!
  }

  def alpha = {
    additionTargets("alpha") === alphas.tail.map(_.id)
  }

  def beta = {
    additionTargets("beta") === betas.map(_.id)
  }

  def gamma = {
    additionTargets("gamma") === gammas.map(_.id)
  }
}

class AdvancedExtSchemaTest
extends ExtSchemaTest
{
  def is = s2"""
  Sync of pending actions to a backend

  apply names to multiple records in a table $alphaNames
  apply multiple changed foreign keys to a record $beta
  apply multiple changed associations to a record $gammaAssoc
  apply changed fields to a record $alphaValues
  create no new pending actions $pendingActions
  delete one record $deleteGamma
  """

  import Schema.Exports._
  import Schema.pending.Exports._
  import profile.api._

  override val responses = {
    val a1 = Alpha.Mapper("response_alpha_1", Flag.Off, 5.0, oid("a1"))
    val a2 = Alpha.Mapper("response_alpha_2", Flag.Off, 5.0, oid("a2"))
    val b1 = Beta.Mapper("response_beta_1", None, oid("a2"), oid("a1"),
      oid("b1"))
    val b2 = Beta.Mapper("response_beta_2", None, oid("a2"), oid("a2"),
      oid("b2"))
    val c = Gamma.Mapper("response_gamma_1", List(oid("b1"), oid("b2")),
      List(), oid("c1"))
    val c2 = Gamma.Mapper("response_gamma_2", List(), List(), oid("c2"))
    a1.js ::
    a2.js ::
    Seq(a1, a2).js ::
    b1.js ::
    b2.js ::
    Seq(b1, b2).js ::
    c.js ::
    c2.js ::
    Seq(c).js ::
    Nil
  }

  def beforeAll() {
    resetDb()
    val (a, b, bId, c, a3) = models
    sync()
  }

  def alphaNames = {
    val names = Alpha.map(_.name).!!
    names must_== Set("response_alpha_1", "response_alpha_2")
  }

  def beta = {
    val ids = Beta.one.!! map { b => (b.alpId, b.alp2Id) }
    ids must beSome((oid("a2"), oid("a1")))
  }

  def gammaAssoc = {
    val betas = Gamma.one.!! map {
      c => (c.bets.all.!!, c.bet2s.all.!!) } map {
      case (b1, b2) => (b1.toList.ids, b2.toList.ids) }
    betas must beSome((Set(oid("b1"), oid("b2")), Set()))
  }

  def alphaValues = {
    Alpha.one.map(_.map(_.flog)).!! must beSome(Flag.Off)
  }

  def pendingActions = {
    Update.!! must be empty
  }

  def deleteGamma = {
    Gamma.!!.toList.ids must_== List(oid("c1"))
  }
}
