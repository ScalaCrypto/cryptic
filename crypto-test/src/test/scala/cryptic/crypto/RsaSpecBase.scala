package cryptic
package crypto

import cryptic.codec.{Chill, Fst, Upickle}
import cryptic.{Codec, Decrypt, Encrypt}
import upickle.default.*

import java.security.{PrivateKey, PublicKey}
import scala.util.Try

trait RsaSpecBase extends CryptoSpecBase:
  private val keyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  override given encrypt: Encrypt[Try] = Rsa.encrypt
  override given decrypt: Decrypt[Try] = Rsa.decrypt
  override def toString: String = "RSAChill"

class RsaChillSpec extends RsaSpecBase:
  override given stringCodec: Codec[String] = Chill.codec

class RsaFstSpec extends RsaSpecBase:
  override given stringCodec: Codec[String] = Fst.codec

class RsaUpickleSpec[V: ReadWriter] extends RsaSpecBase:
  override given stringCodec: Codec[String] = Upickle.codec
