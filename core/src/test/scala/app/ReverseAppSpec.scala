package app

import cryptic.Encrypted
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class ReverseAppSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.{*, given}
  import cryptic.codec.default.given
  import cryptic.crypto.demo.Reverse.given
  import Functor.tryFunctor

  val clear = "secret"
  val encrypted: Encrypted[Try, String] = clear.encrypted
  val decrypted: Try[String] = encrypted.decrypted

  "Cryptic" should "encrypt" in:
    encrypted.bytes.success.value.toSeq should not equal clear.getBytes.toSeq

  "Cryptic" should "decrypt" in:
    decrypted.success.value shouldEqual clear

  case class Person(id: Long, email: Encrypted[Try, String])

  "Person" should "handle encryption" in:
    val id = 17
    val email = "martin@scalacrypto.org"
    val person = Person(id, email.encrypted)
    person.email.bytes should not equal email.getBytes()
    person.email.contains(email).success shouldBe Success(true)
    person.toString shouldBe "Person(17,Encrypted(Success(CipherText(0x67726f2e6f7470797263616c616373406e697472616d))))"

  "PlainText" should "be easy" in:
    PlainText(clear).bytes shouldBe clear.getBytes()
    val enc = clear.encrypted
    enc.bytes.success.value shouldBe clear.getBytes.reverse
