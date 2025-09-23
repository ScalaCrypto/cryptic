package cryptic

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

case class Name(literal: String)

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

case class EmailAddress(literal: String)

case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class EncryptedSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.default.given
  import cryptic.crypto.Reverse.given

  private val clear = "nisse"
  private val enc1: Encrypted[String] = clear.encrypted
  private val emptyString: Encrypted[String] = Encrypted.empty[String, Id]

  "Case class with encrypted members" should "encrypt and decrypt" in:
    case class Foo(clear: String, secret: Encrypted[String])
    val foo = Foo("clear", "secret".encrypted)
    foo.secret.bytes shouldEqual Array(116, 101, 114, 99, 101, 115)
    foo.secret.decrypted shouldEqual Success("secret")
  "Encrypted bytes" should "be callable without decrypt in scope" in:
    val e = "secret".encrypted
    e.bytes shouldEqual Array(116, 101, 114, 99, 101, 115)
  "Encrypted" should "have same value in encrypted space" in:
    val enc2 = clear.encrypted
    enc1 shouldEqual enc2
  "Encrypted" should "not equal different values" in:
    val enc2 = "kalle".encrypted
    (enc1 == enc2) shouldBe false
  "Encrypted" should "have exists" in:
    enc1.exists(_ == clear) shouldBe Success(true)
    enc1.exists(_ == "kalle") shouldBe Success(false)
  "Encrypted" should "have forall" in:
    enc1.forall(_ == clear) shouldBe Success(true)
    enc1.forall(_ == "kalle") shouldBe Success(false)
  "Encrypted" should "have foreach" in:
    var a = ""
    enc1.foreach(a = _)
    a shouldBe clear
  "Encrypted" should "be mappable" in:
    enc1.map(_.toUpperCase).decrypted shouldBe Success("NISSE")
  "Encrypted" should "be flat-mappable" in:
    enc1
      .flatMap(v =>
        val str: String = v.take(2)
        val enc2 = str.encrypted
        enc2
      )
      .decrypted shouldBe Success("ni")
  "Encrypted" should "be collectable" in:
    enc1
      .collect:
        case `clear` => "nice!"
      .decrypted shouldBe Success("nice!")
    enc1
      .collect:
        case "kalle" => "nice!"
      .run[Id, Try] shouldBe emptyString
    enc1
      .collect:
        case "kalle" => "nice!"
      .decrypted
      .failure
      .exception
      .getMessage shouldBe "decrypted called on collected empty"
  "Encrypted" should "be filterable" in:
    enc1.filter(_.length > 2).decrypted shouldBe Success(clear)
    enc1.filter(_.length < 2).run shouldBe emptyString
  "Encrypted" should "be foldable" in:
    enc1.fold("kalle")(_.toUpperCase) shouldBe Success("NISSE")
    enc1
      .filter(_.length > 10)
      .run
      .fold("kalle")(_.toUpperCase) shouldBe Success("kalle")
    enc1
      .filter(_.length < 10)
      .run
      .fold("kalle")(_.toUpperCase) shouldBe Success("NISSE")
  "Encrypted" should "be decryptable with alternative" in:
    enc1.filter(_.length > 2).decryptedOrElse("kalle") shouldBe Success(clear)
    enc1.filter(_.length < 2).decryptedOrElse("kalle") shouldBe Success("kalle")
  "Encrypted" should "be or-elsable" in:
    enc1
      .filter(_.length > 2)
      .orElse("kalle".encrypted)
      .decrypted shouldBe Success(clear)
    enc1
      .filter(_.length < 2)
      .orElse("kalle".encrypted)
      .decrypted shouldBe Success("kalle")
