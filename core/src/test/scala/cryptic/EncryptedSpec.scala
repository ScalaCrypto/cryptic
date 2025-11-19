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
  import cryptic.cipher.demo.Reverse.given
  import cryptic.Functor.tryFunctor

  private val clear = "nisse"
  private val encNisse: Encrypted[Try, String] = clear.encrypted
  private val encEmpty: Encrypted[Try, String] = Encrypted.empty[Try, String]

  "Encrypted empty" should "have isEmpty" in:
    encEmpty.isEmpty.success shouldBe Success(true)
    encEmpty.nonEmpty.success shouldBe Success(false)
  "Encrypted empty" should "not decrypt" in:
    encEmpty.decrypted.failure
  "Case class with encrypted members" should "encrypt and decrypt" in:
    case class Foo(clear: String, secret: Encrypted[Try, String])
    val foo = Foo("clear", "secret".encrypted)
    foo.secret.bytes.success.value.toSeq shouldEqual Seq(116, 101, 114, 99, 101,
      115)
    foo.secret.decrypted shouldEqual Success("secret")
  "Encrypted bytes" should "be callable without decrypt in scope" in:
    val e = "secret".encrypted
    e.bytes.success.value.toSeq shouldEqual Seq(116, 101, 114, 99, 101, 115)
  "Encrypted" should "have same value in encrypted space" in:
    val enc2 = clear.encrypted
    encNisse shouldEqual enc2
  "Encrypted" should "not equal different values" in:
    val enc2 = "kalle".encrypted
    (encNisse == enc2) shouldBe false
  "Encrypted" should "have exists" in:
    encNisse.exists(_ == clear).success shouldBe Success(true)
    encNisse.exists(_ == "kalle").success shouldBe Success(false)
  "Encrypted" should "have forall" in:
    encNisse.forall(_ == clear).success shouldBe Success(true)
    encNisse.forall(_ == "kalle").success shouldBe Success(false)
  "Encrypted" should "have foreach" in:
    var a = ""
    encNisse.foreach(a = _)
    a shouldBe clear
  "Encrypted" should "be mappable" in:
    encNisse.map(_.toUpperCase).decrypted.success shouldBe Success("NISSE")
  "Encrypted" should "be flat-mappable" in:
    encNisse
      .flatMap(v =>
        val str: String = v.take(2)
        val enc2 = str.encrypted
        enc2
      )
      .decrypted
      .success shouldBe Success("ni")
  "Encrypted" should "be collectable" in:
    encNisse
      .collect:
        case `clear` => "nice!"
      .decrypted
      .success shouldBe Success("nice!")
    encNisse
      .collect:
        case "kalle" => "nice!"
      .run shouldBe encEmpty
    val dec = encNisse
      .collect:
        case "kalle" => "nice!"
      .decrypted
    dec.failure
    dec.failed.get.getMessage shouldBe "decrypted called on collected empty"
  "Encrypted" should "be filterable" in:
    encNisse.filter(_.length > 2).decrypted.success shouldBe Success(clear)
    val empty = encNisse.filter(_.length < 2).run
    empty shouldBe encEmpty
    empty.isEmpty.success shouldBe Success(true)
    empty.nonEmpty.success shouldBe Success(false)
  "Encrypted" should "be foldable" in:
    encNisse.fold("kalle")(_.toUpperCase).success shouldBe Success("NISSE")
    encNisse
      .filter(_.length > 2)
      .run
      .fold("kalle")(_.toUpperCase)
    encEmpty.fold("kalle")(_.toUpperCase).success shouldBe Success("kalle")
    encNisse
      .filter(_.length < 2)
      .run
      .fold("kalle")(_.toUpperCase)
      .success shouldBe Success("kalle")
  "Encrypted" should "be decryptable with alternative" in:
    encNisse
      .filter(_.length > 2)
      .decryptedOrElse("kalle")
      .success shouldBe Success(clear)
    encNisse
      .filter(_.length < 2)
      .decryptedOrElse("kalle")
      .success shouldBe Success("kalle")
  "Encrypted" should "be or-elsable" in:
    encNisse
      .filter(_.length > 2)
      .orElse("kalle".encrypted)
      .decrypted
      .success shouldBe Success(clear)
    encNisse
      .filter(_.length < 2)
      .orElse("kalle".encrypted)
      .decrypted
      .success shouldBe Success("kalle")
