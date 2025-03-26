package cryptic
package serialization

import cryptic.syntax.RichAny
import cryptic.{ crypto, Cryptic, Encrypted }
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default._

import scala.util.Success

case class Foo(bar: String)

object Foo {
  implicit val rw: ReadWriter[Foo] = macroRW[Foo] // Only need for Upickle
}

case class FooBar(secret: Encrypted[String])

class ChillEncryptedSpec extends EncryptedSpecBase {
  override def serializer[V](implicit rw: ReadWriter[V]): Serializer[V] =
    Chill.serializer
}

class FstEncryptedSpec extends EncryptedSpecBase {
  override def serializer[V](implicit rw: ReadWriter[V]): Serializer[V] =
    Fst.serializer
}

class UpickleEncryptedSpec extends EncryptedSpecBase {
  override def serializer[V](implicit rw: ReadWriter[V]): Serializer[V] =
    Upickle[V]
}

trait EncryptedSpecBase extends AnyFlatSpec with Matchers with EitherValues {

  import crypto.Reverse._
  implicit def serializer[V](implicit rw: ReadWriter[V] = null): Serializer[V]

  "Case class with encrypted members" should "encrypt and decrypt" in {
    val foo = FooBar("secret".encrypted)
    foo.secret.bytes shouldEqual serializer[String].serialize("secret").reverse
    foo.secret.decrypted shouldEqual Success("secret")

  }
  "Encrypted case class with" should "encrypt and decrypt" in {
    val foo = Foo("clear")
    val encryptedFoo = foo.encrypted(encrypt)
    val plainText = serializer[Foo].serialize(foo)
    encryptedFoo.bytes shouldBe plainText.reverse // Reveres crypto
    encryptedFoo.decrypted shouldEqual Success(foo)
  }
  "Pending operations " should " be ran when decrypting" in {
    val encrypted: Encrypted[String] = "secret".encrypted
    val pending: Cryptic[String] = encrypted.map(_.toUpperCase)
    pending.decrypted shouldEqual Success("SECRET")
  }
  "Encrypted without decryption key" should "have same value in encrypted space" in {
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("nisse")
    enc1 shouldEqual enc2
  }
  "Encrypted values" should "not be equal" in {
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  }
}
