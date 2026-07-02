
package cryptic
package cipher

import scala.util.{Failure, Success, Try}

/** One-Time Password (OTP) algorithm implementation.
  *
  * This implementation uses the One-Time Pad cipher, which provides
  * information-theoretic security when the pad is truly random, at least as
  * long as the message, and used only once.
  */
object Otp:
  given functor: Functor[Try] = Functor.tryFunctor
  val version: Version = FixedVersion(0, 0, 0, 1)

  object default:
    export cryptic.default.{given, *}
    export Otp.{given, *}

  /** Represents a pad for the OTP cipher.
    *
    * @param bytes
    *   The pad bytes. Must be at least as long as the message to be encrypted.
    */
  case class Pad(bytes: IArray[Byte])

  given encrypt(using pad: Pad): Encrypt[Try] =
    case plainText if plainText.aad.nonEmpty =>
      Failure(new UnsupportedOperationException("OTP does not support AAD"))
    case plainText if pad.bytes.length < plainText.bytes.length =>
      Failure(new IllegalArgumentException("Pad is shorter than message"))
    case plainText =>
      Success(
        CipherText(
          version.bytes,
          plainText.bytes.xor(pad.bytes)
        )
      )

  given decrypt(using pad: Pad): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(v, encryptedBytes) if version.supports(v) =>
        if pad.bytes.length < encryptedBytes.length then
          Failure(
            new IllegalArgumentException("Pad is shorter than ciphertext")
          )
        else Success(PlainText(encryptedBytes.xor(pad.bytes), AAD.empty))
      case IArray(v, _) =>
        version.failed(v)
