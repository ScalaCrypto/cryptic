package app

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.{Success, Try}

class DefaultAppSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.cipher.default.{*, given}

  val keyPair: KeyPair = newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  val clear = "secret"
  val encrypted: Encrypted[Try, String] = clear.encrypted
  val decrypted: Try[String] = encrypted.decrypted

  it should "encrypt" in:
    encrypted.bytes.success should not equal Success(clear.getBytes)

  it should "decrypt" in:
    decrypted.success shouldEqual Success(clear)

  case class Person(id: Long, email: Encrypted[Try, String])

  val id = 17
  val email = "martin@scalacrypto.org"
  val person: Person = Person(id, email.encrypted)

  "Email" should "be encrypted" in:
    person.email.bytes.success should not equal Success(email.getBytes())
    person.email.contains(email) shouldBe Success(true)
    person.toString should startWith("Person(17,Encrypted(Success(CipherText(0x")

  "Email" should "be decrypted" in:
    person.email.decrypted.success shouldEqual Success(email)
