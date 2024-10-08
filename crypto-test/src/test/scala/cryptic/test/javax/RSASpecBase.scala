package cryptic
package test
package javax

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.RSA
import cryptic.serialization.{Chill, Fst, Serializer, Upickle}
import cryptic.test.CryptoSpecBase
import upickle.default

import java.security.{PrivateKey, PublicKey}

abstract class RSASpecBase extends CryptoSpecBase {
  private val keyPair = RSA.keygen(512)
  implicit val publicKey: PublicKey = keyPair.getPublic
  implicit val privateKey: PrivateKey = keyPair.getPrivate
  val encrypt: Encrypt = RSA.encrypt
  val decrypt: Decrypt = RSA.decrypt
  override def toString: String = "RSAChill"
}

class RSAChillSpec extends RSASpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Chill.serializer
}

class RSAFstSpec extends RSASpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Fst.serializer
}

class RSAUpickleSpec extends RSASpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Upickle[V]
}
