package cryptic
package cipher

import cryptic.*
import cryptic.codec.default.given
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.compiletime.deferred
import scala.util.{Success, Try}

trait CipherSpecBase extends AnyFlatSpecLike with Matchers with TryValues:
  given encrypt: Encrypt[Try] = deferred
  given decrypt: Decrypt[Try] = deferred
  given stringCodec: Codec[String] = deferred
  given functor: Functor[Try] = Functor.tryFunctor

  "case class with encrypted members" should "encrypt and decrypt" in:
    case class Foo(clear: String, secret: Encrypted[Try, String])
    val foo =
      Foo("clear", "secret".encrypted)
    foo.secret.bytes.success shouldNot be(Success(null))
    foo.secret.decrypted shouldEqual Success("secret")
  "Encrypted bytes" should "be callable without decrypt in scope" in:
    val encrypted = "secret".encrypted
    encrypted.bytes.success shouldNot be(Success(null))
  "Encrypted same plain text " should "have different cipher text " in:
    given e: Encrypt[Try] = encrypt
    val enc1 = "nisse".encrypted
    val enc2 = "nisse".encrypted
    enc1 shouldNot equal(enc2)
  "Different encrypted plain texts" should "have different encrypted values" in:
    given e: Encrypt[Try] = encrypt
    val enc1 = "nisse".encrypted
    val enc2 = "kalle".encrypted
    (enc1 == enc2) shouldBe false
  "Pending operations " should " be ran when decrypting" in:
    val encrypted: Encrypted[Try, String] =
      "secret".encrypted
    val pending: Cryptic[Try, String] = encrypted.map(_.toUpperCase)
    given d: Decrypt[Try] = decrypt
    pending.decrypted shouldEqual Success("SECRET")
  "Filter" should "filter" in:
    val encrypted: Encrypted[Try, String] = "secret".encrypted
    val filtered = encrypted.filter(_ != "secret")
    filtered.run.isEmpty.success.value shouldBe true
    encrypted
      .filter(_ == "secret")
      .decrypted
      .success shouldEqual Success("secret")
  "Map" should "map" in:
    val encrypted: Encrypted[Try, String] =
      "secret".encrypted
    val mapped = encrypted.map(_.toUpperCase)
    mapped.decrypted.success shouldEqual Success("SECRET")
  "Flatmap" should "flatmap" in:
    "secret".encrypted
      .flatMap(_.toUpperCase.encrypted)
      .decrypted
      .success shouldEqual Success("SECRET")
