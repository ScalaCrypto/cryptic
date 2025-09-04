package app

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}

class EllipticCurveAppSpec extends AnyFlatSpec with Matchers with ScalaFutures:
  import cryptic.{*, given}
  import cryptic.codec.default.given
  import cryptic.crypto.EllipticCurve.{*, given}
  import scala.concurrent.ExecutionContext.Implicits.global

  val keyPair: KeyPair = newKeyPair()
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate

  val clear = "secret"
  val encrypted: Encrypted[String] = clear.encrypted.futureValue
  val decrypted: String = encrypted.decrypted.futureValue

  "Cryptic" should "encrypt" in:
    encrypted.bytes should not equal clear.getBytes

  "Cryptic" should "decrypt" in:
    decrypted shouldEqual clear

  case class Person(id: Long, email: Encrypted[String])

  "Person" should "handle encryption" in:
    val id = 17
    val email = "martin@scalacrypto.org"
    val person = Person(id, email.encrypted.futureValue)
    person.email.bytes should not equal email.getBytes()
    person.email.contains(email).futureValue shouldBe true
    person.toString should startWith("Person(17,Encrypted(CipherText(0x")
