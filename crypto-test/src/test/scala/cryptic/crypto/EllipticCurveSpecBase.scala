package cryptic
package crypto

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.EllipticCurve
import cryptic.codec.{Chill, Fst, Upickle}
import upickle.default.{ReadWriter, Writer, Reader, given}
import java.security.{KeyPair, PrivateKey, PublicKey}

trait EllipticCurveSpecBase extends CryptoSpecBase:
  private val keyPair: KeyPair = EllipticCurve.newKeyPair()
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  override given encrypt: Encrypt = EllipticCurve.encrypt
  override given decrypt: Decrypt = EllipticCurve.decrypt

class EllipticCurveChillSpec extends EllipticCurveSpecBase:
  override given stringCodec: Codec[String] = Chill.codec
  override def toString: String = "ECChill"

class EllipticCurveFstSpec extends EllipticCurveSpecBase:
  override given stringCodec: Codec[String] = Fst.codec
  override def toString: String = "ECFst"

class EllipticCurveUpickleSpec extends EllipticCurveSpecBase:
  override given stringCodec: Codec[String] = Upickle.codec[String]
  override def toString: String = "ECUpickle"
