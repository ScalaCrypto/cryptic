package cryptic
package cipher
package demo

import scala.util.{Failure, Success, Try}

/** The Caesar object provides encryption and decryption functionality using
 * the Caesar cipher. It supports transforming `PlainText` to `CipherText` and
 * vice versa based on a configurable key offset.
 *
 * The implementation defines given instances for the `Encrypt` and `Decrypt`
 * type classes using the `Try` effect type for error handling.
 *
 * Key Features:
 *   - The `encrypt` operation shifts bytes in plaintext by the positive key
 *     offset to produce encrypted ciphertext.
 *   - The `decrypt` operation reverses the shift using the negative key offset
 *     to produce the original plaintext.
 *   - Offset shifting is performed using the `addOffset` extension method.
 */
object Caesar:
  case class Key(offset: Int)

  given encrypt(using key: Key): Encrypt[Try] = (plainText: PlainText) =>
    plainText.bytes.addOffset(key.offset).map(CipherText.apply)

  given decrypt(using key: Key): Decrypt[Try] = (cipherText: CipherText) =>
    cipherText.bytes.addOffset(-key.offset).map(PlainText.apply)

  extension (bytes: IArray[Byte])
    def addOffset(offset: Int): Try[IArray[Byte]] =
      if offset == 0 then
        Failure(new IllegalArgumentException("Key offset cannot be 0"))
      else Success(bytes.mutable.map(b ⇒ (b + offset).toByte).immutable)
