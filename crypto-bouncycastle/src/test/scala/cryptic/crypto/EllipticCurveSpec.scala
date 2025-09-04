package cryptic
package crypto

import org.bouncycastle.jcajce.provider.util.BadBlockException
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.concurrent.ExecutionContext

class EllipticCurveSpec extends AnyFlatSpec with Matchers with ScalaFutures:
  import cryptic.codec.default.given
  import EllipticCurve.{*, given}

  given ec: ExecutionContext = ExecutionContext.global
  private val keyPair: KeyPair = EllipticCurve.newKeyPair()
  given publicKey: PublicKey = keyPair.getPublic
  private val text = "secret"
  val enc: Encrypted[String] = text.encrypted.futureValue

  "EllipticCurve" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    given privateKey: PrivateKey = keyPair.getPrivate
    enc.decrypted.futureValue shouldEqual text

  "EllipticCurve" should "fail with the wrong key" in:
    given privateKey: PrivateKey = EllipticCurve.newKeyPair().getPrivate
    try
      enc.decrypted
      fail("expected failure")
    catch
      case ex: BadBlockException =>
        ex.getMessage shouldBe "unable to process block"
      case x => fail(s"unexpected $x")

  "EllipticCurve" should "handle large data" in:
    given privateKey: PrivateKey = keyPair.getPrivate
    val large = "secret" * 1000
    val enc = large.encrypted.futureValue
    enc.decrypted.futureValue shouldBe large

  "EllipticCurve" should "hide the plain text" in:
    new String(enc.bytes.mutable)
      .contains(text.getBytes()) shouldBe false
