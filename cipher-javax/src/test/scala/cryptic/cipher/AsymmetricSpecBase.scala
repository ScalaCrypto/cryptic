package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.compiletime.deferred
import scala.util.Try

trait AsymmetricSpecBase extends AnyFlatSpec with Matchers with TryValues:
  given functor: Functor[Try] = deferred
  given stringCodec: Codec[String] = deferred

  val keyPair: KeyPair
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate

  val text = "secret"
