package app

import cryptic.crypto.Aes.{AesParams, GcmParams}
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.{Success, Try}

class DefaultAppSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.default.{given, *}

  val keyPair: KeyPair = newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  given aesParams: AesParams = GcmParams()
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
  val person = Person(id, email.encrypted)

  "Email" should "be encrypted" in:
    person.email.bytes.unsafeArray should not equal email.getBytes()
    person.email.contains(email) shouldBe true
    person.toString should startWith("Person(17,Encrypted(CipherText(0x")

  "Email" should "be decrypted" in:
    person.email.decrypted.success shouldEqual Success(email)
