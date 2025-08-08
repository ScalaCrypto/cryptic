package cryptic
package codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

case class Name(literal: String)
case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
case class EmailAddress(literal: String)
case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class FstSpec extends AnyFlatSpec with Matchers:
  import Fst.{*, given}

  "Fst codec" should "encode string and then decode back to original string" in:
    val plainText = "kalle".encoded
    plainText shouldNot equal(PlainText("kalle"))
    val actual = plainText.decoded
    actual shouldEqual Success("kalle")

  "Fst codec" should "encode user and decode back to original user" in:
    val user = User(
      id = 1,
      alias = "kalle",
      name = PersonName(first = Name("Karl"), last = Name("Nilsson")),
      email = EmailAddress("kalle@nilsson.se")
    )
    val plainText = user.encoded
    val actual = plainText.decoded
    actual shouldEqual Success(user)
