package cryptic
package test
package javax

import cryptic.codec.{Chill, Fst, Upickle}
import cryptic.crypto.RSA
import cryptic.test.CryptoSpecBase
import cryptic.{Decrypt, Encrypt}
import upickle.default.*

import java.security.{PrivateKey, PublicKey}

trait RSASpecBase extends CryptoSpecBase:
  private val keyPair = RSA.keygen(512)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  override given encrypt: Encrypt = RSA.encrypt
  override given decrypt: Decrypt = RSA.decrypt
  override def toString: String = "RSAChill"

class RSAChillSpec extends RSASpecBase:
  override given stringCodec: Codec[String] = Chill.codec

class RSAFstSpec extends RSASpecBase:
  override given stringCodec: Codec[String] = Fst.codec

class RSAUpickleSpec[V: ReadWriter] extends RSASpecBase:
  override given stringCodec: Codec[String] = Upickle.codec
