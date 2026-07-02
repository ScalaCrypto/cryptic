
package cryptic
package cipher

/** One-Time Password (OTP) algorithm implementation.
  *
  * This implementation uses the One-Time Pad cipher, which provides
  * information-theoretic security when the pad is truly random, at least as
  * long as the message, and used only once.
  */
object Otp:
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

  given encrypt[F[_]](using pad: Pad, functor: Functor[F]): Encrypt[F] =
    case plainText if plainText.aad.nonEmpty =>
      new UnsupportedOperationException("OTP does not support AAD").failed
    case plainText if pad.bytes.length < plainText.bytes.length =>
      new IllegalArgumentException("Pad is shorter than message").failed
    case plainText =>
      CipherText(
        version.bytes,
        plainText.bytes.xor(pad.bytes)
      )
        .pure

  given decrypt[F[_]](using pad: Pad, functor: Functor[F]): Decrypt[F] =
    (_: CipherText).splitWith:
      case IArray(v, encryptedBytes) if version.supports(v) =>
        if pad.bytes.length < encryptedBytes.length then
          new IllegalArgumentException("Pad is shorter than ciphertext").failed
        else PlainText(encryptedBytes.xor(pad.bytes), AAD.empty).pure
      case IArray(v, _) =>
        version.failed[PlainText](v).exception.failed
