package app

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

class AesAppSpec extends AnyFlatSpec with Matchers with TryValues:

  import cryptic.{*, given}
  import cryptic.codec.default.given
  import cryptic.crypto.Aes.{*, given}

  given passphrase: Passphrase = Passphrase("correct horse")
  val clear = "secret"
  val encrypted: Encrypted[String] = clear.encrypted
  val decrypted: Try[String] = encrypted.decrypted

  "Cryptic" should "encrypt" in:
    encrypted.bytes should not equal clear.getBytes

  "Cryptic" should "decrypt" in:
    decrypted.success shouldEqual Success(clear)

  case class Person(id: Long, email: Encrypted[String])

  val id = 17
  val email = "martin@scalacrypto.org"
  val person: Person = Person(id, email.encrypted)

  "Email" should "be encrypted" in:
    person.email.bytes.unsafeArray should not equal email.getBytes()
    person.email.contains(email) shouldBe true
    person.toString should startWith("Person(17,Encrypted(CipherText(0x")

  "Email" should "be decrypted" in:
    person.email.decrypted.success shouldEqual Success(email)
