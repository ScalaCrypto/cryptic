package cryptic

import cryptic.syntax._
import org.scalatest._

class EncryptedSpec extends FlatSpec with Matchers with EitherValues {

  import crypto.Reverse._
  import serialization.Fst._

  "Case class with encrypted members" should "encrypt and decrypt" in {
    case class Foo(clear: String, secret: Encrypted[String])
    val foo = Foo("clear", "secret".encrypted)
    foo.secret.bytes shouldEqual Array(116, 101, 114, 99, 101, 115, 6, -4)
    // Only need decryption function when decrypting
    foo.secret.decrypted shouldEqual Right("secret")
  }
  "Encrypted" should "have same value in encrypted space equal without decryption key" in {
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("nisse")
    enc1 shouldEqual enc2
  }
  "Encrypted" should "not equal different values" in {
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  }
}
