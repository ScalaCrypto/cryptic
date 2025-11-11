package cryptic

import cryptic.codec.*
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*

import scala.util.{Success, Try}

case class Foo(bar: String)

object Foo:
  given rw: ReadWriter[Foo] = macroRW[Foo] // Only need for Upickle

case class FooBar(secret: Encrypted[Try, String])

trait EncryptedSpecBase extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.default.{*, given}
  import cryptic.cipher.demo.Reverse.{*, given}
  import cryptic.Functor.tryFunctor

  given codec[V: ReadWriter]: Codec[V] = scala.compiletime.deferred
  "Case class with encrypted members" should "encrypt and decrypt" in:
    val foo = FooBar("secret".encrypted)
    foo.secret.bytes.success.value shouldEqual "secret".encoded.bytes.reverse
    foo.secret.decrypted.success.value shouldEqual "secret"
  "Encrypted case class with" should "encrypt and decrypt" in:
    val foo = Foo("clear")
    val encryptedFoo = foo.encrypted
    val plainText = foo.encoded
    encryptedFoo.bytes.success.value shouldBe plainText.bytes.reverse // Reveres crypto
    encryptedFoo.decrypted shouldEqual Success(foo)
  "Pending operations " should "run when decrypting" in:
    val encrypted: Encrypted[Try, String] = "secret".encrypted
    val pending: Cryptic[Try, String] = encrypted.map(_.toUpperCase)
    pending.decrypted shouldEqual Success("SECRET")
  "Encrypted without a decryption key" should "have the same value in encrypted space" in:
    val enc1 = "nisse".encrypted
    val enc2 = "nisse".encrypted
    enc1 shouldEqual enc2
  "Encrypted values" should "not be equal" in:
    val enc1 = "nisse".encrypted
    val enc2 = "kalle".encrypted
    (enc1 == enc2) shouldBe false
