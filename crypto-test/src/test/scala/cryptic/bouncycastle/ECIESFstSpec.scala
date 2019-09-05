package cryptic.bouncycastle
import java.security.{KeyPair, PrivateKey, PublicKey}

import cryptic.crypto.ECIES
import cryptic.{Decrypt, Encrypt, FstSpecBase}

class ECIESFstSpec extends FstSpecBase {
  private val keyPair: KeyPair = ECIES.keygen(256)
  implicit private val publicKey: PublicKey = keyPair.getPublic
  implicit private val privateKey: PrivateKey = keyPair.getPrivate

  val encrypt: Encrypt = ECIES.encrypt
  val decrypt: Decrypt = ECIES.decrypt
  override def toString: String = "ECIESFst"
}
