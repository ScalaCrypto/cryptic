package cryptic

import cryptic.codec.*
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*

import scala.concurrent.Future
import scala.util.Success

case class Foo(bar: String)

object Foo:
  given rw: ReadWriter[Foo] = macroRW[Foo] // Only need for Upickle

case class FooBar(secret: Encrypted[String])

trait EncryptedSpecBase extends support.AsyncTestBase:
  import cryptic.codec.default.{*, given}
  import cryptic.crypto.Reverse.{*, given}
  given codec[V: ReadWriter]: Codec[V] = scala.compiletime.deferred

  private val enc: Encrypted[String] = "secret".encrypted.futureValue
  
  "Case class with encrypted members" should "encrypt and decrypt" in:
    val foo = FooBar(enc)
    foo.secret.bytes shouldEqual "secret".encoded.bytes.reverse
    foo.secret.decrypted.futureValue shouldEqual "secret"
  "Encrypted case class with" should "encrypt and decrypt" in:
    val foo = Foo("clear")
    val encryptedFoo = foo.encrypted.futureValue
    val plainText = foo.encoded
    encryptedFoo.bytes shouldBe plainText.bytes.reverse // Reveres crypto
    encryptedFoo.decrypted.futureValue shouldEqual foo
  "Pending operations " should "run when decrypting" in:
    val pending: Cryptic[String] = enc.map(_.toUpperCase)
    pending.decrypted.futureValue shouldEqual "SECRET"
  "Encrypted without a decryption key" should "have the same value in encrypted space" in:
    val enc1 = "nisse".encrypted.futureValue
    val enc2 = "nisse".encrypted.futureValue
    enc1 shouldEqual enc2
  "Encrypted values" should "not be equal" in:
    val enc1 = "nisse".encrypted.futureValue
    val enc2 = "kalle".encrypted.futureValue
    (enc1 == enc2) shouldBe false
