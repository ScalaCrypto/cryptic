package cryptic
package test

import cryptic.serialization.Serializer
import cryptic.{Cryptic, Decrypt, Encrypt, Encrypted}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import upickle.default.ReadWriter

trait CryptoSpecBase extends AnyFlatSpecLike with Matchers with EitherValues {
  val encrypt: Encrypt
  val decrypt: Decrypt
//  import cryptic.serialization.Chill._
  import cryptic.syntax._
  implicit def serializer[V](implicit rw: ReadWriter[V] = null): Serializer[V]

  "case class with encrypted members" should "encrypt and decrypt" in {
    case class Foo(clear: String, secret: Encrypted[String])
    val foo = {
      Foo("clear", "secret".encrypted(encrypt))
    }
    foo.secret.bytes shouldNot be(null)
    foo.secret.decrypted(decrypt) shouldEqual Right("secret")
  }
  "Encrypted bytes" should "be callable without decrypt in scope" in {
    implicit val e: Encrypt = encrypt
    val encrypted = Encrypted[String]("secret")
    encrypted.bytes shouldNot be(null)
  }
  "Encrypted same plain text " should "have different cipher text " in {
    implicit val e: Encrypt = encrypt
    val enc1 = "nisse".encrypted
    val enc2 = "nisse".encrypted
    enc1 shouldNot equal(enc2)
  }
  "Different encrypted plain texts" should "have different encrypted values" in {
    implicit val e: Encrypt = encrypt
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  }
  "Pending operations " should " be ran when decrypting" in {
    val encrypted: Encrypted[String] = {
      "secret".encrypted(encrypt)
    }
    val pending: Cryptic[String] = encrypted.map(_.toUpperCase)
    implicit val d: Decrypt = decrypt
    pending.decrypted shouldEqual Right("SECRET")
  }
  "Filter" should "filter" in {
    val encrypted: Encrypted[String] = {
      "secret".encrypted(encrypt)
    }
    val filtered = encrypted.filter(_ != "secret")
    filtered.decrypted(decrypt) shouldEqual Left("decrypted called on filtered empty")
    encrypted.filter(_ == "secret").decrypted(decrypt) shouldEqual Right("secret")
  }
  "Map" should "map" in {
    val encrypted: Encrypted[String] = {
      "secret".encrypted(encrypt)
    }
    val mapped = encrypted.map(_.toUpperCase)
    mapped.decrypted(decrypt) shouldEqual Right("SECRET")
  }
  "Flatmap" should "flatmap" in {
    implicit val e: Encrypt = encrypt
    val encrypted: Encrypted[String] = "secret".encrypted
    val flatMapped = encrypted.flatMap(_.toUpperCase.encrypted)
    flatMapped.decrypted(decrypt) shouldEqual Right("SECRET")
  }
}
