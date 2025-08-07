package cryptic
package codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

case class Name(literal: String)
case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
case class EmailAddress(literal: String)
case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class ChillSpec extends AnyFlatSpec with Matchers:
  import Chill.*
  import Chill.given

  "Chill codec" should "encode string and then decode back to original string" in:
    val plainText = codec.encode("kalle")
    plainText shouldNot equal(PlainText("kalle"))
    val actual = codec.decode(plainText)
    actual shouldEqual Success("kalle")
  "Chill codec" should "encode user and decode back to original user" in:
    val user = User(
      id = 1,
      alias = "kalle",
      name = PersonName(first = Name("Karl"), last = Name("Nilsson")),
      email = EmailAddress("kalle@nilsson.se")
    )
    val plainText = codec.encode(user)
    val actual = codec.decode(plainText)
    actual shouldEqual Success(user)
