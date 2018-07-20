package cryptic.javax

import java.security.{PrivateKey, PublicKey}

import cryptic.crypto.RSA
import cryptic.{Decrypt, Encrypt, FstSpecBase}

class RSAFstSpec extends FstSpecBase {
  private val keyPair = RSA.keygen(512)
  implicit val publicKey: PublicKey = keyPair.getPublic
  implicit val privateKey: PrivateKey = keyPair.getPrivate
  val encrypt: Encrypt = RSA.encrypt
  val decrypt: Decrypt = RSA.decrypt
  override def toString: String = "RSAFst"
}
