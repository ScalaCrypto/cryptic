package cryptic
package codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues
import upickle.default.{*, given}

import scala.util.{Success, Try}

case class Name(literal: String)

object Name:
  given rw: ReadWriter[Name] = macroRW[Name]

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

object PersonName:
  given rw: ReadWriter[PersonName] = macroRW[PersonName]

case class EmailAddress(literal: String)

object EmailAddress:
  given rw: ReadWriter[EmailAddress] = macroRW[EmailAddress]
case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

object User:
  given rw: ReadWriter[User] = macroRW[User]
class UpickleSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.Upickle.{*, given}
  val user: User = User(
    id = 1,
    alias = "kalle",
    name = PersonName(first = Name("Karl"), last = Name("Nilsson")),
    email = EmailAddress("kalle@nilsson.se")
  )

  "Upickle codec" should "encode string and then decode back to original string" in:
    val plainText = "kalle".encoded
    plainText shouldNot equal(PlainText("kalle"))
    val actual: Try[String] = plainText.decoded
    actual shouldEqual Success("kalle")
  "Upickle codec" should "encode a case class and decode back to original case class" in:
    val plainText = user.encoded
    val actual: Try[User] = plainText.decoded
    actual shouldEqual Success(user)
  "Upickle codec" should "be usable by the encoder/decoder" in:
    import cryptic.cipher.demo.Reverse.*
    import cryptic.cipher.demo.Reverse.given
    import cryptic.Functor.tryFunctor
    val encrypted = user.encrypted
    encrypted.bytes.success.value shouldBe write(
      user
    ).getBytes.reverse // Reverse...
    val decrypted = encrypted.decrypted
    decrypted.success.value shouldBe user
