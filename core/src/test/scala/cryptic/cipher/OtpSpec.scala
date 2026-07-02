package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class OtpSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.default.given
  import Otp.{*, given}

  private val text = "hello"
  private val padBytes = IArray(1, 2, 3, 4, 5).map(_.toByte)
  given Pad(padBytes)

  "Otp Encrypted" should "support encryption and decryption" in:
    val encrypted = text.encrypted
    encrypted.decrypted.success.value shouldEqual text

  it should "not support AAD" in:
    val aad = "my-aad".aad
    val encrypted = text.encrypted(aad)
    intercept[UnsupportedOperationException]:
      encrypted.bytes.get

  it should "fail if pad is too short" in:
    val shortPad = Pad(IArray(1, 2).map(_.toByte))
    given Pad = shortPad
    text.encrypted.bytes.failure.exception shouldBe a[IllegalArgumentException]

  it should "fail on wrong version" in:
    val badVersion = FixedVersion(9, 9, 9, 9)
    val ct =
      CipherText(badVersion.bytes, IArray(0).map(_.toByte))
    val encrypted = Encrypted[Try, String](Success(ct))
    encrypted.decrypted.failure.exception.getMessage should include(
      "Unsupported version 9.9.9.9"
    )

  it should "be XOR based" in:
    // "h" is 104 (0x68), pad is 1 -> 105 (0x69) "i"
    // "e" is 101 (0x65), pad is 2 -> 103 (0x67) "g"
    // "l" is 108 (0x6c), pad is 3 -> 111 (0x6f) "o"
    // "l" is 108 (0x6c), pad is 4 -> 104 (0x68) "h"
    // "o" is 111 (0x6f), pad is 5 -> 106 (0x6a) "j"
    val encrypted = text.encrypted
    encrypted.splitWith:
      case IArray(_, encryptedBytes) => Success(new String(encryptedBytes.mutable))
    .success.value shouldBe "igohj"
