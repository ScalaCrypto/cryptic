package cryptic

import cryptic.codec.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*

import scala.util.Success

case class Foo(bar: String)

object Foo:
  implicit val rw: ReadWriter[Foo] = macroRW[Foo] // Only need for Upickle

case class FooBar(secret: Encrypted[String])

class ChillEncryptedSpec extends EncryptedSpecBase:
  override def codec[V](implicit rw: ReadWriter[V]): Codec[V] = Chill.codec

class FstEncryptedSpec extends EncryptedSpecBase:
  override def codec[V: ReadWriter]: Codec[V] = Fst.codec

class UpickleEncryptedSpec extends EncryptedSpecBase:
  override def codec[V: ReadWriter]: Codec[V] = Upickle.codec

trait EncryptedSpecBase extends AnyFlatSpec with Matchers with EitherValues:
  import cryptic.codec.default.{*, given}
  import cryptic.crypto.Reverse.{*, given}
  implicit def codec[V: ReadWriter]: Codec[V]
  "Case class with encrypted members" should "encrypt and decrypt" in:
    val foo = FooBar("secret".encrypted)
    foo.secret.bytes shouldEqual "secret".encoded.bytes.reverse
    foo.secret.decrypted shouldEqual Success("secret")
  "Encrypted case class with" should "encrypt and decrypt" in:
    val foo = Foo("clear")
    val encryptedFoo = foo.encrypted
    val plainText = foo.encoded
    encryptedFoo.bytes shouldBe plainText.bytes.reverse // Reveres crypto
    encryptedFoo.decrypted shouldEqual Success(foo)
  "Pending operations " should " be ran when decrypting" in:
    val encrypted: Encrypted[String] = "secret".encrypted
    val pending: Cryptic[String] = encrypted.map(_.toUpperCase)
    pending.decrypted shouldEqual Success("SECRET")
  "Encrypted without decryption key" should "have same value in encrypted space" in:
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("nisse")
    enc1 shouldEqual enc2
  "Encrypted values" should "not be equal" in:
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
