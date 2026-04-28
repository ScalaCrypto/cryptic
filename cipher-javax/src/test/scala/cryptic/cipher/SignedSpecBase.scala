package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec

import scala.compiletime.deferred
import scala.util.{Failure, Success, Try}

trait SignedSpecBase extends AsymmetricSpecBase:
  given sign: Sign[Try] = deferred
  given verify: Verify[Try] = deferred

  "signing" should "support signing and verifying" in:
    val signed = text.signed
    signed.verified shouldBe Success(text)

  it should "fail verification when signed text is altered" in:
    val signed: Signed[Try, String] = text.signed
    val tampered: Try[SignedText] = signed.splitWith:
      case IArray(version, text, sig) =>
        val alteredText = text.mutable
        alteredText(0) = (text.head ^ 0xff).toByte
        Success(SignedText(version, IArray.unsafeFromArray(alteredText), sig))
    Signed[Try, String](tampered).verified match
      case Failure(e) =>
        e shouldBe a[IllegalArgumentException]
        e.getMessage shouldBe "Signature verification failed"
      case Success(result) =>
        fail(
          s"Expected verification to fail on tampered data, but got: $result"
        )

  it should "reject unsupported signature version" in:
    val tampered = text.signed.splitWith:
      case IArray(_, bytes, sig) =>
        val wrongVersion = FixedVersion(0, 0, 0, 0)
        Success(SignedText(wrongVersion.bytes, bytes, sig))
    Signed[Try, String](tampered).verified match
      case Failure(e) =>
        e shouldBe a[IllegalArgumentException]
        e.getMessage should include("Unsupported version 0.0.0.0")
      case x =>
        fail(s"Expected verification to fail due to version mismatch, got $x")
