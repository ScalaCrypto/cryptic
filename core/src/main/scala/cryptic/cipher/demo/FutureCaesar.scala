package cryptic
package cipher
package demo

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

/**
 * The FutureCaesar object provides encryption and decryption functionality
 * using the Caesar cipher with asynchronous computation via the `Future`
 * effect type.
 *
 * It defines given instances for the type classes `Encrypt` and `Decrypt`
 * using the `Future` effect type, enabling asynchronous encryption and decryption.
 *
 * Key Features:
 *   - The `Key` case class encapsulates the offset used for shifting bytes in the cipher.
 *   - The `encrypt` implementation shifts the bytes in plaintext by the positive key offset
 *     to produce encrypted ciphertext.
 *   - The `decrypt` implementation reverses the shift using the negative key offset
 *     to produce the original plaintext.
 *   - Byte offset manipulation is supported via the `addOffset` extension method on `IArray[Byte]`.
 *
 * Encryption and decryption involve the following operations:
 *   - `Encrypt[Future]` and `Decrypt[Future]` instances leverage the specified key offset and
 *     perform computations asynchronously within a given `ExecutionContext`.
 *   - The `addOffset` extension method facilitates byte shifting for encryption and decryption,
 *     returning the result within a `Future`.
 */
object FutureCaesar:
  case class Key(offset: Int)

  given encrypt(using key: Key, ec: ExecutionContext): Encrypt[Future] =
    (plainText: PlainText) =>
      plainText.bytes.addOffset(key.offset).map(CipherText.apply)

  given decrypt(using key: Key, ec: ExecutionContext): Decrypt[Future] =
    (cipherText: CipherText) =>
      cipherText.bytes.addOffset(-key.offset).map(PlainText.apply)

  extension (bytes: IArray[Byte])
    def addOffset(offset: Int): Future[IArray[Byte]] =
      if offset == 0 then
        Future.failed(new IllegalArgumentException("Key offset cannot be 0"))
      else
        Future.successful(bytes.mutable.map(b ⇒ (b + offset).toByte).immutable)
