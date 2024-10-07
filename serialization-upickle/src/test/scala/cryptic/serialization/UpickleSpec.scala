package cryptic
package serialization

import cryptic.syntax.RichAny
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default._

case class Name(literal: String)

object Name {
  implicit val rw: ReadWriter[Name] = macroRW[Name]
}

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

object PersonName {
  implicit val rw: ReadWriter[PersonName] = macroRW[PersonName]
}

case class EmailAddress(literal: String)

object EmailAddress {
  implicit val rw: ReadWriter[EmailAddress] = macroRW[EmailAddress]
}
case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

object User {
  implicit val rw: ReadWriter[User] = macroRW[User]
}
class UpickleSpec extends AnyFlatSpec with Matchers {
  val user: User = User(id = 1, alias = "kalle", name = PersonName(first = Name("Karl"), last = Name("Nilsson")), email = EmailAddress("kalle@nilsson.se"))

  "Upickle serializer" should "serialize string and then deserialize back to original string" in {
    val serializer: Serializer[String] = Upickle[String]
    val pt = serializer.serialize("kalle")
    pt shouldNot equal(PlainText("kalle"))
    val o = serializer.deserialize(pt)
    o shouldEqual Right("kalle")
  }

  "Upickle serializer" should "serialize a case class and deserialize back to original case class" in {
    val serializer = Upickle[User]
    val pt = serializer.serialize(user)
    val o = serializer.deserialize(pt)
    o shouldEqual Right(user)
  }
  "Upickle serializer" should "be usable by the encoder/decoder" in {
    import cryptic.crypto.Reverse._
    implicit val serializer: Serializer[User] = Upickle[User]
    val encrypted = user.encrypted
    encrypted.bytes shouldBe write(user).getBytes.reverse // Reverse...
    val decrypted = encrypted.decrypted
    decrypted shouldBe Right(user)
  }
}
