//package cryptic.crypto
//
//import cryptic.{encrypted, Decrypt, Encrypt}
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//import java.security.{KeyPair, PrivateKey, PublicKey}
//import scala.util.Success
//
//trait AsymmetricAesBase extends AnyFlatSpec with Matchers:
//  import cryptic.codec.default.given
//
//  given asymmetric: AsymmetricAes = scala.compiletime.deferred
//  given encrypt: Encrypt = summon[Encrypt]
//  given decrypt: Decrypt = summon[Decrypt]
//
//  val keyPair: KeyPair = asymmetric.keygen(2048)
//  given publicKey: PublicKey = keyPair.getPublic
//  val text: String = "secret" * 10000 // Large data
//  given aesParams: Aes.GcmParams = Aes.GcmParams()
//  "AsymmetricAes" should "support encryption and decryption" in:
//    // Note no need for the private key when encrypting
//    val encrypted = text.encrypted
//
//    given privateKey: PrivateKey = keyPair.getPrivate
//    encrypted.decrypted match
//      case Success(actual) => actual shouldEqual text
//      case x ⇒ fail(s"does not decrypt: $x")
//
//  "AsymmetricAes" should "hide plaintext" in:
//    new String(text.encrypted.bytes.mutable)
//      .contains(text.getBytes()) shouldBe false
