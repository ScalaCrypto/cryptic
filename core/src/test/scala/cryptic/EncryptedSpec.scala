package cryptic

import cryptic.syntax._
import org.scalatest._

case class Name(literal: String)

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

case class EmailAddress(literal: String)

case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class EncryptedSpec extends FlatSpec with Matchers with EitherValues {

  import serialization.StringSerializer._

  "Case class with encrypted members" should "encrypt and decrypt" in {
    import crypto.Reverse.encrypt
    case class Foo(clear: String, secret: Encrypted[String])
    val foo = Foo("clear", "secret".encrypted)
    foo.secret.bytes shouldEqual Array(116, 101, 114, 99, 101, 115)
    // Only need decryption function when decrypting
    import crypto.Reverse.decrypt
    foo.secret.decrypted shouldEqual Right("secret")
  }
  "Encrypted bytes" should "be callable without decrypt in scope" in {
    import crypto.Reverse.encrypt
    val e = Encrypted[String]("secret")
    e.bytes shouldEqual Array(116, 101, 114, 99, 101, 115)
  }
  "Encrypted" should "have same value in encrypted space equal without decryption key" in {
    import crypto.Reverse.encrypt
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("nisse")
    enc1 shouldEqual enc2
  }
  "Encrypted" should "not equal different values" in {
    import crypto.Reverse.encrypt
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  }
}
