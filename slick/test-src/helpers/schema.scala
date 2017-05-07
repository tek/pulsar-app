package tryp.slick.test

import concurrent.ExecutionContext.Implicits.global

import tryp.slick.sync.{SyncSchema, BackendSync}

abstract class SchemaSpecBase
extends SlickSpecBase
{
  val Schema: SyncSchema

  import profile.api._

  lazy val pas = Schema.pendingActions

  def resetDb() {
    Schema.resetDb()
  }

  val objectIds = scala.collection.mutable.Map[String, ObjectId]()

  def oid(name: String) = objectIds.getOrElseUpdate(name, ObjectId.generate)

  def additionTargets(model: String) = {
    val act = pas.filter(_.model === model).get()
    val add = act.flatMap(_.additions.all.get())
    add map(_.target)
  }

  def additions(model: String) = {
    val adds = for {
      a <- pas if a.model === model
      add <- a.additions
    } yield add
    adds.get()
  }

  implicit class `json shortcut`[A: EncodeJson](a: A) {
    def js = a.asJson.spaces2
  }

  val responses: List[String] = Nil

  implicit lazy val rest = new TestRestClient(responses)

  def sync() = {
    BackendSync()(Schema).!!
  }
}
