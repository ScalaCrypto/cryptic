package app

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.{Success, Try}

class EllipticCurveAppSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.{*, given}
  import cryptic.codec.default.given
  import cryptic.crypto.EllipticCurve.{*, given}

  val keyPair: KeyPair = newKeyPair()
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate

  val clear = "secret"
  val encrypted: Encrypted[String] = clear.encrypted
  val decrypted: Try[String] = encrypted.decrypted

  "Cryptic" should "encrypt" in:
    encrypted.bytes should not equal clear.getBytes

  "Cryptic" should "decrypt" in:
    decrypted.success shouldEqual Success(clear)

  case class Person(id: Long, email: Encrypted[String])

  "Person" should "handle encryption" in:
    val id = 17
    val email = "martin@scalacrypto.org"
    val person = Person(id, email.encrypted)
    person.email.bytes should not equal email.getBytes()
    person.email.contains(email) shouldBe true
    person.toString should startWith("Person(17,Encrypted(CipherText(0x")
