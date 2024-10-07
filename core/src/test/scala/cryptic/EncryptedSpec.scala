package cryptic

import cryptic.syntax._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

case class Name(literal: String)

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

case class EmailAddress(literal: String)

case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class EncryptedSpec extends AnyFlatSpec with Matchers with EitherValues {

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
  "Encrypted" should "be mappable" in {
    import crypto.Reverse._
    Encrypted("nisse").map(_.toUpperCase).decrypted shouldBe Right("NISSE")
  }
  "Encrypted" should "be flat-mappable" in {
    import crypto.Reverse._
    Encrypted("nisse").flatMap(_.take(2).encrypted).decrypted shouldBe Right(
      "ni"
    )
  }
  "Encrypted" should "be collectable" in {
    import crypto.Reverse._
    Encrypted("nisse").collect { case "nisse" =>
      "nice!"
    }.decrypted shouldBe Right("nice!")
    Encrypted("nisse").collect { case "kalle" =>
      "nice!"
    }.run shouldBe Right(Encrypted.empty[String])
  }
  "Encrypted" should "be filterable" in {
    import crypto.Reverse._
    Encrypted("nisse").filter(_.length > 2).decrypted shouldBe Right("nisse")
    Encrypted("nisse").filter(_.length < 2).run shouldBe Right(
      Encrypted.empty[String]
    )
  }
  "Encrypted" should "be foldable" in {
    import crypto.Reverse._
    Encrypted("nisse").fold("kalle") {
      _.toUpperCase
    } shouldBe Right("NISSE")
    Encrypted("nisse").filter(_.length > 10).run.flatMap {
      _.fold("kalle") {
        _.toUpperCase
      }
    } shouldBe Right("kalle")
    Encrypted("nisse").filter(_.length < 10).run.flatMap {
      _.fold("kalle") {
        _.toUpperCase
      }
    } shouldBe Right("NISSE")
  }
  "Encrypted" should "be decryptable with alternative" in {
    import crypto.Reverse._
    Encrypted("nisse")
      .filter(_.length > 2)
      .decryptedOrElse("kalle") shouldBe "nisse"
    Encrypted("nisse")
      .filter(_.length < 2)
      .decryptedOrElse("kalle") shouldBe "kalle"
  }
  "Encrypted" should "be or-elsable" in {
    import crypto.Reverse._
    Encrypted("nisse")
      .filter(_.length > 2)
      .orElse("kalle".encrypted)
      .decrypted shouldBe Right("nisse")
    Encrypted("nisse")
      .filter(_.length < 2)
      .orElse("kalle".encrypted)
      .decrypted shouldBe Right("kalle")
  }
}
