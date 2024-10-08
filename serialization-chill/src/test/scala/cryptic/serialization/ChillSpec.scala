package cryptic
package serialization

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

case class Name(literal: String)
case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
case class EmailAddress(literal: String)
case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class ChillSpec extends AnyFlatSpec with Matchers {
  import Chill._

  "Chill serializer" should "serialize string and then deserialize back to original string" in {
    val plainText = serializer.serialize("kalle")
    plainText shouldNot equal(PlainText("kalle"))
    val actual = serializer.deserialize(plainText)
    actual shouldEqual Success("kalle")
  }

  "Chill serializer" should "serialize user and deserialize back to original user" in {
    val user = User(
      id = 1,
      alias = "kalle",
      name = PersonName(first = Name("Karl"), last = Name("Nilsson")),
      email = EmailAddress("kalle@nilsson.se"))
    val plainText = serializer.serialize(user)
    val actual = serializer.deserialize(plainText)
    actual shouldEqual Success(user)
  }
}
