package tryp.spray

class Exception(what: String)
extends java.lang.RuntimeException(what)

case class UserNotFound(id: String)
extends Exception(s"user not found: $id")

case class ProductNotFound(id: String)
extends Exception(s"product not found: $id")

case class AttrError(obj: ObjectId, name: String, attr: ObjectId)
extends Exception(s"No ${name} with id ${attr} in object ${obj}")

case class AttrTypeError(obj: ObjectId, name: String)
extends Exception(
  s"No embedded data with name ${name} in object ${obj.stringify}")

case class UserAttrTypeError(user: ObjectId, name: String)
extends Exception(
  s"No embedded data with name ${name} in user ${user.stringify}")

case class UserAttrError(user: ObjectId, name: String, attr: ObjectId)
extends Exception(
  s"No ${name} with id ${attr.stringify} in user ${user.stringify}")
