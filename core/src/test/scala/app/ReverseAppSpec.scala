package app

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class ReverseAppSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.{*, given}
  import cryptic.crypto.Reverse.given

  val clear = "secret"
  val encrypted: Encrypted[String] = Encrypted(clear)
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
    person.toString shouldBe "Person(17,Encrypted(CipherText(0x67726f2e6f7470797263616c616373406e697472616d)))"

  "PlainText" should "be easy" in:
    PlainText(clear) shouldBe clear.getBytes()
    val enc = clear.encrypted
    enc.bytes shouldBe clear.getBytes.reverse

