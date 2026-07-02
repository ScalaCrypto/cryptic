
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

  private def xor(bytes: IArray[Byte], pad: IArray[Byte]): IArray[Byte] =
    val b = bytes.mutable
    val k = pad.mutable
    val res = new Array[Byte](b.length)
    for i <- b.indices do res(i) = (b(i) ^ k(i)).toByte
    res.immutable

  given encrypt(using pad: Pad): Encrypt[Try] = (plainText: PlainText) =>
    if plainText.aad.nonEmpty then
      Failure(new UnsupportedOperationException("OTP does not support AAD"))
    else if pad.bytes.length < plainText.bytes.length then
      Failure(new IllegalArgumentException("Pad is shorter than message"))
    else
      Success(
        CipherText(
          version.bytes,
          xor(plainText.bytes, pad.bytes)
        )
      )

  given decrypt(using pad: Pad): Decrypt[Try] = (cipherText: CipherText) =>
    cipherText.splitWith:
      case IArray(v, encryptedBytes) if version.supports(v) =>
        if pad.bytes.length < encryptedBytes.length then
          Failure(
            new IllegalArgumentException("Pad is shorter than ciphertext")
          )
        else Success(PlainText(xor(encryptedBytes, pad.bytes), AAD.empty))
      case IArray(v, _) =>
        version.failed(v)
