package cryptic
package cipher

import cryptic.codec.{Fst, Upickle}
import cryptic.{Codec, Decrypt, Encrypt}
import upickle.default.*

import java.security.{PrivateKey, PublicKey}
import scala.util.Try

trait RsaSpecBase extends CipherSpecBase with SignerSpecBase:
  private val keyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  override given encrypt: Encrypt[Try] = Rsa.encrypt
  override given decrypt: Decrypt[Try] = Rsa.decrypt
  override given sign: Sign[Try] = Rsa.sign
  override given verify: Verify[Try] = Rsa.verify

class RsaFstSpec extends RsaSpecBase:
  override given stringCodec: Codec[String] = Fst.codec
  override given fooCodec: Codec[Foo] = Fst.codec

class RsaUpickleSpec extends RsaSpecBase:
  override given stringCodec: Codec[String] = Upickle.codec
  override given fooCodec: Codec[Foo] = Upickle.codec
