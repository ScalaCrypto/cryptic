package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec

import java.security.KeyPair
import javax.crypto.IllegalBlockSizeException
import scala.util.{Success, Try}

class RsaSpec extends EncryptionSpecBase with SignedSpecBase:

  import Rsa.default.*

  override val keyPair: KeyPair = Rsa.newKeyPair(2048)
  override given encrypt: Encrypt[Try] = Rsa.encrypt
  override given decrypt: Decrypt[Try] = Rsa.decrypt
  given version: Version = FixedVersion(0, 0, 0, 1)
  override given sign: Sign[Try] = Rsa.sign
  override given verify: Verify[Try] = Rsa.verify
  override given functor: Functor[Try] = Functor.tryFunctor
  override given stringCodec: cryptic.Codec[String] =
    Rsa.default.given_Codec_String

  "RSA" should "fail on large data" in:
    ("secret" * 1000).encrypted.cipherText.failure.exception shouldBe a[
      IllegalBlockSizeException
    ]
