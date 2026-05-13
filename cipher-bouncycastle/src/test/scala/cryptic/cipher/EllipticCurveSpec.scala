package cryptic
package cipher

import java.security.KeyPair
import scala.util.{Success, Try}

class EllipticCurveSpec extends EncryptionSpecBase with SignedSpecBase:
  import EllipticCurve.default.*
  override given encrypt: Encrypt[Try] = EllipticCurve.encrypt
  override given decrypt: Decrypt[Try] = EllipticCurve.decrypt
  given version: Version = EllipticCurve.version
  override given sign: Sign[Try] = EllipticCurve.sign
  override given verify: Verify[Try] = EllipticCurve.verify
  override given stringCodec: cryptic.Codec[String] =
    EllipticCurve.default.given_Codec_String

  val keyPair: KeyPair = EllipticCurve.newKeyPair()

  "EllipticCurve" should "handle large data" in:
    val large = "secret" * 1000
    val enc = large.encrypted
    enc.decrypted shouldBe Success(large)
