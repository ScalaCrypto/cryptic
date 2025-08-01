package cryptic
package test
package bouncycastle

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.EC
import cryptic.serialization.{Chill, Fst, Upickle}
import cryptic.test.CryptoSpecBase
import upickle.default.*

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.compiletime.deferred

trait ECSpecBase extends CryptoSpecBase:
  private val keyPair: KeyPair = EC.keygen(256)
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  override given encrypt: Encrypt = EC.encrypt
  override given decrypt: Decrypt = EC.decrypt

class ECChillSpec extends ECSpecBase:
  override given serializer[V]: Serializer[V] = Chill.serializer
  override def toString: String = "ECChill"

class ECFstSpec extends ECSpecBase:
  override given serializer[V]: Serializer[V] = Fst.serializer
  override def toString: String = "ECFst"

class ECUpickleSpec[V: ReadWriter] extends ECSpecBase:
  override given serializer[W]: Serializer[W] =
    Upickle[W]()(using summon[ReadWriter[V]].asInstanceOf[ReadWriter[W]])
  override def toString: String = "ECUpickle"
