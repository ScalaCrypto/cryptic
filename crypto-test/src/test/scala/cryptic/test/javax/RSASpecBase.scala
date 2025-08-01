package cryptic
package test
package javax

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.RSA
import cryptic.serialization.{Chill, Fst, Upickle}
import cryptic.test.CryptoSpecBase
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
  override given serializer[V]: Serializer[V] = Chill.serializer

class RSAFstSpec extends RSASpecBase:
  override given serializer[V]: Serializer[V] = Fst.serializer

class RSAUpickleSpec[V: ReadWriter] extends RSASpecBase:
  override given serializer[W]: Serializer[W] =
    Upickle[W]()(using summon[ReadWriter[V]].asInstanceOf[ReadWriter[W]])
