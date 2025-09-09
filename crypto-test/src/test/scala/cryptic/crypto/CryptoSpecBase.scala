package cryptic
package crypto

import cryptic.*
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.compiletime.deferred
import scala.util.Success

trait CryptoSpecBase extends support.AsyncTestBase:
  given encrypt: Encrypt = deferred
  given decrypt: Decrypt = deferred
  given stringCodec: Codec[String] = deferred

  "case class with encrypted members" should "encrypt and decrypt" in:
    case class Foo(clear: String, secret: Encrypted[String])
    val foo =
      Foo("clear", "secret".encrypted.futureValue)
    foo.secret.bytes.mutable shouldNot be(null)
    foo.secret.decrypted.futureValue shouldEqual "secret"
  "Encrypted bytes" should "be callable without decrypt in scope" in:
    given e: Encrypt = encrypt
    val encrypted = "secret".encrypted.futureValue
    encrypted.bytes.mutable shouldNot be(null)
  "Encrypted same plain text " should "have different cipher text " in:
    given e: Encrypt = encrypt
    val enc1 = "nisse".encrypted
    val enc2 = "nisse".encrypted
    enc1 shouldNot equal(enc2)
  "Different encrypted plain texts" should "have different encrypted values" in:
    given e: Encrypt = encrypt
    val enc1 = "nisse".encrypted
    val enc2 = "kalle".encrypted
    (enc1 == enc2) shouldBe false
  "Pending operations " should " be ran when decrypting" in:
    val encrypted: Encrypted[String] =
      "secret".encrypted.futureValue
    val pending: Cryptic[String] = encrypted.map(_.toUpperCase)
    given d: Decrypt = decrypt
    pending.decrypted.futureValue shouldEqual "SECRET"
  "Filter" should "filter" in:
    val encrypted: Encrypted[String] =
      "secret".encrypted.futureValue
    val filtered = encrypted.filter(_ != "secret")
    filtered.run.map(_.isEmpty).futureValue shouldBe true
    encrypted
      .filter(_ == "secret")
      .decrypted
      .futureValue shouldEqual "secret"
  "Map" should "map" in:
    val encrypted: Encrypted[String] =
      "secret".encrypted.futureValue
    val mapped = encrypted.map(_.toUpperCase)
    mapped.decrypted.futureValue shouldEqual "SECRET"
  "Flatmap" should "flatmap" in:
    given e: Encrypt = encrypt
    val encrypted: Encrypted[String] = "secret".encrypted.futureValue
    val flatMapped = encrypted.flatMap(_.toUpperCase.encrypted.futureValue)
    flatMapped.decrypted.futureValue shouldEqual "SECRET"
