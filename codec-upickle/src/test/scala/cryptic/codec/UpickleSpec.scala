package cryptic
package codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*

import scala.util.Success

case class Name(literal: String)

object Name:
  implicit val rw: ReadWriter[Name] = macroRW[Name]

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

object PersonName:
  implicit val rw: ReadWriter[PersonName] = macroRW[PersonName]

case class EmailAddress(literal: String)

object EmailAddress:
  implicit val rw: ReadWriter[EmailAddress] = macroRW[EmailAddress]
case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

object User:
  implicit val rw: ReadWriter[User] = macroRW[User]
class UpickleSpec extends AnyFlatSpec with Matchers:
  val user: User = User(
    id = 1,
    alias = "kalle",
    name = PersonName(first = Name("Karl"), last = Name("Nilsson")),
    email = EmailAddress("kalle@nilsson.se")
  )

  "Upickle codec" should "encode string and then decode back to original string" in:
    val codec: Codec[String] = Upickle[String]()
    val plainText = codec.encode("kalle")
    plainText shouldNot equal(PlainText("kalle"))
    val actual = codec.decode(plainText)
    actual shouldEqual Success("kalle")
  "Upickle codec" should "encode a case class and decode back to original case class" in:
    val codec = Upickle[User]()
    val plainText = codec.encode(user)
    val actual = codec.decode(plainText)
    actual shouldEqual Success(user)
  "Upickle codec" should "be usable by the encoder/decoder" in:
    import cryptic.crypto.Reverse.*
    import cryptic.crypto.Reverse.given
    implicit val codec: Codec[User] = Upickle[User]()
    val encrypted = user.encrypted
    encrypted.bytes shouldBe write(user).getBytes.reverse // Reverse...
    val decrypted = encrypted.decrypted
    decrypted shouldBe Success(user)
