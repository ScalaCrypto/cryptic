package cryptic

import java.security.{PrivateKey, PublicKey}

import org.scalatest._

class RSAFstSpec extends FlatSpec with Matchers with EitherValues {

  import cryptic.crypto.RSA._
  import cryptic.serialization.Fst._
  import cryptic.syntax._

  private val keyPair = keygen(512)
  "case class with encrypted members" should "encrypt and decrypt" in {
    case class Foo(clear: String, secret: Encrypted[String])
    val foo = {
      implicit val publicKey: PublicKey = keyPair.getPublic
      Foo("clear", "secret".encrypted)
    }
    // We don't need the public key to get bytes
    foo.secret.bytes shouldNot be(null)
    // Only need decryption function when decrypting
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    foo.secret.decrypted shouldEqual Right("secret")
  }
  "Encrypted bytes" should "be callable without decrypt in scope" in {
    implicit val publicKey: PublicKey = keyPair.getPublic
    val e = Encrypted[String]("secret")
    e.bytes shouldNot be(null)
  }
  "Encrypted same plain text " should "have different cipher text " in {
    implicit val publicKey: PublicKey = keyPair.getPublic
    val enc1 = "nisse".encrypted
    val enc2 = "nisse".encrypted
    enc1 shouldNot equal(enc2)
  }
  "Encrypted" should "not equal different values" in {
    implicit val publicKey: PublicKey = keyPair.getPublic
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  }
  "Pending operations " should " be ran when decrypting" in {
    val encrypted: Encrypted[String] = {
      implicit val publicKey: PublicKey = keyPair.getPublic
      "secret".encrypted
    }
    val pending: Cryptic[String] = encrypted.map(_.toUpperCase)
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    pending.decrypted shouldEqual Right("SECRET")
  }
  "Filter" should "filter" in {
    val encrypted: Encrypted[String] = {
      implicit val publicKey: PublicKey = keyPair.getPublic
      "secret".encrypted
    }
    val filtered = encrypted.filter(_ != "secret")
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    filtered.decrypted shouldEqual Left("decrypted called on filtered empty")
    encrypted.filter(_ == "secret").decrypted shouldEqual Right("secret")
  }
  "Map" should "map" in {
    val encrypted: Encrypted[String] = {
      implicit val publicKey: PublicKey = keyPair.getPublic
      "secret".encrypted
    }
    val mapped = encrypted.map(_.toUpperCase)
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    mapped.decrypted shouldEqual Right("SECRET")
  }
  "Flatmap" should "flatmap" in {
    implicit val publicKey: PublicKey = keyPair.getPublic
    val encrypted: Encrypted[String] = "secret".encrypted
    val flatMapped = encrypted.flatMap(_.toUpperCase.encrypted)
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    flatMapped.decrypted shouldEqual Right("SECRET")
  }
}
