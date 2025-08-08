package cryptic
package test
package bouncycastle

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.EC
import cryptic.codec.{Chill, Fst, Upickle}
import cryptic.test.CryptoSpecBase
import upickle.default.{ReadWriter, Writer, Reader, given}
import java.security.{KeyPair, PrivateKey, PublicKey}

trait ECSpecBase extends CryptoSpecBase:
  private val keyPair: KeyPair = EC.keygen(256)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  override given encrypt: Encrypt = EC.encrypt
  override given decrypt: Decrypt = EC.decrypt

class ECChillSpec extends ECSpecBase:
  override given stringCodec: Codec[String] = Chill.codec
  override def toString: String = "ECChill"

class ECFstSpec extends ECSpecBase:
  override given stringCodec: Codec[String] = Fst.codec
  override def toString: String = "ECFst"

class ECUpickleSpec extends ECSpecBase:
  override given stringCodec: Codec[String] = Upickle.codec[String]
  override def toString: String = "ECUpickle"
