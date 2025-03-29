package cryptic
package test

import cryptic.serialization.Serializer
import cryptic.{Cryptic, Decrypt, Encrypt, Encrypted}
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import upickle.default.ReadWriter

import scala.compiletime.deferred
import scala.util.Success

trait CryptoSpecBase extends AnyFlatSpecLike with Matchers with TryValues:
  import cryptic.syntax.*
  given encrypt: Encrypt = deferred
  given decrypt: Decrypt = deferred
  given serializer[V]: Serializer[V] = deferred

  "case class with encrypted members" should "encrypt and decrypt" in:
    case class Foo(clear: String, secret: Encrypted[String])
    val foo =
      Foo("clear", "secret".encrypted)
    foo.secret.bytes shouldNot be(null)
    foo.secret.decrypted shouldEqual Success("secret")
  "Encrypted bytes" should "be callable without decrypt in scope" in:
    implicit val e: Encrypt = encrypt
    val encrypted = Encrypted[String]("secret")
    encrypted.bytes shouldNot be(null)
  "Encrypted same plain text " should "have different cipher text " in:
    implicit val e: Encrypt = encrypt
    val enc1 = "nisse".encrypted
    val enc2 = "nisse".encrypted
    enc1 shouldNot equal(enc2)
  "Different encrypted plain texts" should "have different encrypted values" in:
    implicit val e: Encrypt = encrypt
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  "Pending operations " should " be ran when decrypting" in:
    val encrypted: Encrypted[String] =
      "secret".encrypted
    val pending: Cryptic[String] = encrypted.map(_.toUpperCase)
    implicit val d: Decrypt = decrypt
    pending.decrypted shouldEqual Success("SECRET")
  "Filter" should "filter" in:
    val encrypted: Encrypted[String] =
      "secret".encrypted
    val filtered = encrypted.filter(_ != "secret")
    filtered.run.map(_.isEmpty).success shouldBe Success(true)
    encrypted
      .filter(_ == "secret")
      .decrypted
      .success shouldEqual Success("secret")
  "Map" should "map" in:
    val encrypted: Encrypted[String] =
      "secret".encrypted
    val mapped = encrypted.map(_.toUpperCase)
    mapped.decrypted.success shouldEqual Success("SECRET")
  "Flatmap" should "flatmap" in:
    implicit val e: Encrypt = encrypt
    val encrypted: Encrypted[String] = "secret".encrypted
    val flatMapped = encrypted.flatMap(_.toUpperCase.encrypted)
    flatMapped.decrypted.success shouldEqual Success("SECRET")
