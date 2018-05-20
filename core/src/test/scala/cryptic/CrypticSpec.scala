package cryptic

import org.scalatest._
import syntax._

class CrypticSpec extends FlatSpec with Matchers with EitherValues {
  case class Name(literal: String)
  case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
  case class EmailAddress(literal: String)
  case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

  "Case class with encrypted members" should "encrypt and decrypt" in {
    import crypto.Reverse._
    import serialization.StringSerializer._

    case class Foo(clear: String, secret: Encrypted[String])
    val secret = "secret"
    val terces = "terces"
    val foo = Foo("clear", secret.encrypted)
    foo.secret.bytes shouldEqual terces.getBytes
    // Only need decryption function when decrypting
    foo.secret.decrypted shouldEqual Right("secret")
  }

  "Encrypted bytes" should "be callable without encrypt/decrypt in scope" in {
    e: Encrypted[String] =>
      e.bytes
  }
}
