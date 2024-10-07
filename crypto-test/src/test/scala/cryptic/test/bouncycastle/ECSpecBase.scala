package cryptic
package test
package bouncycastle

import cryptic.crypto.EC
import cryptic.serialization.{Chill, Fst, Serializer, Upickle}
import cryptic.test.CryptoSpecBase
import cryptic.{Decrypt, Encrypt}
import upickle.default

import java.security.{KeyPair, PrivateKey, PublicKey}

abstract class ECSpecBase extends CryptoSpecBase {
  private val keyPair: KeyPair = EC.keygen(256)
  implicit private val publicKey: PublicKey = keyPair.getPublic
  implicit private val privateKey: PrivateKey = keyPair.getPrivate

  val encrypt: Encrypt = EC.encrypt
  val decrypt: Decrypt = EC.decrypt
}

class ECChillSpec extends ECSpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Chill.serializer
  override def toString: String = "ECChill"
}

class ECFstSpec extends ECSpecBase {

  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Fst.serializer
  override def toString: String = "ECFst"

}

class ECUpickleSpec extends ECSpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Upickle[V]
  override def toString: String = "ECUpickle"
}
