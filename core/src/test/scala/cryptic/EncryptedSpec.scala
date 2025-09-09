package cryptic

import cryptic.support.AsyncTestBase
import org.scalactic.Prettifier.default
import org.scalatest.TryValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

case class Name(literal: String)

case class PersonName(first: Name, middle: Option[Name] = None, last: Name)

case class EmailAddress(literal: String)

case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class EncryptedSpec extends AsyncTestBase:
  import cryptic.codec.default.given
  import cryptic.crypto.Reverse.given

  private val clear = "nisse"
  private val enc1: Encrypted[String] = clear.encrypted.futureValue
  private val emptyString: Encrypted[String] = Encrypted.empty[String]

  "Case class with encrypted members" should "encrypt and decrypt" in:
    case class Foo(clear: String, secret: Encrypted[String])
    val foo = Foo("clear", "secret".encrypted.futureValue)
    foo.secret.bytes shouldEqual Array(116, 101, 114, 99, 101, 115)
    foo.secret.decrypted.futureValue shouldEqual "secret"
  "Encrypted bytes" should "be callable without decrypt in scope" in:
    val e = "secret".encrypted.futureValue
    e.bytes shouldEqual Array(116, 101, 114, 99, 101, 115)
  "Encrypted" should "have same value in encrypted space" in:
    val enc2 = clear.encrypted.futureValue
    enc1 shouldEqual enc2
  it should "not equal different values" in:
    val enc2 = "kalle".encrypted
    (enc1 == enc2) shouldBe false
  it should "have exists" in:
    enc1.exists(_ == clear).futureValue shouldBe true
    enc1.exists(_ == "kalle").futureValue shouldBe false
  it should "have forall" in:
    enc1.forall(_ == clear).futureValue shouldBe true
    enc1.forall(_ == "kalle").futureValue shouldBe false
  it should "have foreach" in:
    val p = Promise[String]()
    enc1.foreach(s => p.complete(Success(s)))
    p.future.map(_ shouldBe clear)
  it should "be mappable" in:
    enc1.map(_.toUpperCase).decrypted.futureValue shouldBe "NISSE"
  it should "be flat-mappable" in:
    val fm = enc1.flatMap(s =>  s.take(2).encrypted.futureValue)
    fm.decrypted.futureValue shouldBe "ni"
  it should "be collectable with matching pattern" in:
    enc1
      .collect:
        case `clear` => "nice!"
      .decrypted.futureValue shouldBe "nice!"

  it should "result in empty when pattern doesn't match" in:
    enc1
      .collect:
        case "kalle" => "nice!"
      .run.futureValue shouldBe emptyString

  it should "fail with appropriate error when decrypting empty collected result" in:
    recoverToSucceededIf[UnsupportedOperationException]:
      enc1
        .collect:
          case "kalle" => "nice!"
        .decrypted
  it should "be filterable" in:
    enc1.filter(_.length > 2).decrypted.futureValue shouldBe clear
    enc1.filter(_.length < 2).run.futureValue shouldBe emptyString
  it should "be foldable" in:
    enc1.fold("kalle")(_.toUpperCase).futureValue shouldBe "NISSE"
    enc1
      .filter(_.length > 10)
      .run
      .flatMap(_.fold("kalle")(_.toUpperCase)).futureValue shouldBe "kalle"
    enc1
      .filter(_.length < 10)
      .run
      .flatMap(_.fold("kalle")(_.toUpperCase)).futureValue shouldBe "NISSE"
  it should "be decryptable with alternative" in:
    enc1.filter(_.length > 2).decryptedOrElse("kalle").futureValue shouldBe clear
    enc1.filter(_.length < 2).decryptedOrElse("kalle").futureValue shouldBe "kalle"
  it should "be or-elsable" in:
    enc1
      .filter(_.length > 2)
      .orElse("kalle".encrypted.futureValue)
      .decrypted
      .futureValue shouldBe clear
    enc1
      .filter(_.length < 2)
      .orElse("kalle".encrypted.futureValue)
      .decrypted
      .futureValue shouldBe "kalle"
