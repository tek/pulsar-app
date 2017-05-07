package tryp
package slick
package sync

import monocle._, macros._

import cats._
import cats.implicits._

object Request
{
  trait Component
  {
    def segments: Nel[String]

    def tip = segments.last
  }

  case class Strings(segments: Nel[String])
  extends Component

  case class Attr(id: ObjectId, attr: String)
  extends Component
  {
    def segments = Nel(id.stringify, attr)
  }

  case object Root
  extends Component
  {
    def segments = Nel("")
  }

  implicit def stringToComponent(s: String): Component = Strings(Nel(s))

  implicit def objectIdToComponent(id: ObjectId): Component =
    Strings(Nel(id.stringify))

  def empty = Request(Nel(Root: Component), Map(), None)

  def at(head: String, tail: String*) =
    Request(Nel(Strings(Nel(head, tail: _*)))
      .widen[Component], Map(), None)
}

@Lenses
case class Request(components: Nel[Request.Component],
  params: Params, body: Option[String])
  {
    def path = components.toList
      .map(_.segments.toList.mkString("/"))
      .mkString("/")

    def tip = components.last.tip

    def segments = components flatMap(_.segments)
  }

trait RestClient
{
  def post(req: Request): String Xor String
  def put(req: Request): String Xor String
  def delete(req: Request): String Xor String
  def get(req: Request): String Xor String
}

case class RestAdapter(client: RestClient, request: Request = Request.empty)
{
  def post() = client.post(request)

  def put() = client.put(request)

  def delete() = client.delete(request)

  def get() = client.get(request)

  def ids() = path("ids").get()

  def attrIds(container: ObjectId, attr: String) =
    path(Request.Attr(container, attr)).ids()

  def req(r: Request => Request) = copy(request = r(request))

  def params(par: Params) =
    req(Request.params.modify(a => Monoid[Params].combineAll(List(a, par))))

  def path(comp: Request.Component) =
    req(Request.components.modify(_.combine(Nel(comp))))

  def /(comp: Request.Component) = path(comp)

  def body(content: String) =
    req(Request.body.set(Some(content)))
}
