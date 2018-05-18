//package cryptic
//
//import javax.crypto.SecretKey
//import org.scalatest._
//
//class AESSpec extends FlatSpec with Matchers {
//
//  import crypto.AES._
//  import serialization.FstSerializer._
//
//  implicit private val secretKey: SecretKey = keygen(256)
//
//  "AES Encrypted" should "support encryption and decryption" in {
//    val encrypted = Encrypted[String]("nisse")
//    encrypted.decrypted match {
//      case Right(decrypted) ⇒ decrypted shouldEqual "nisse"
//      case x ⇒ fail(s"does not decrypt: $x")
//    }
//  }
//
//  "AES Encrypted" should "hide plaintext" in {
//    // Note no need for the private key when encrypting
//    Encrypted[String]("nisse") match {
//      case Encrypted.Value(ct) ⇒ new String(ct.bytes).contains("nisse")
//      case _ ⇒ None
//    }
//  }
//
//  "AES" should "same value should be equal in encrypted space without decryption key" in {
//    val enc1 = Encrypted[String]("nisse")
//    val enc2 = Encrypted[String]("nisse")
//    enc1 shouldEqual enc2
//  }
//
//  "AES" should "different values should no be equal" in {
//    val enc1 = Encrypted[String]("nisse")
//    val enc2 = Encrypted[String]("kalle")
//    (enc1 == enc2) shouldBe false
//  }
//
//}
