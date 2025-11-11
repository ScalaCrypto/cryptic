package cryptic
package cipher
package demo

import scala.util.{Success, Try}

/**
 * The Reverse object provides functionality to reverse the byte sequences of plain text
 * and cipher text, both for encryption and decryption. It defines given instances for
 * the Encrypt and Decrypt type classes using Try for error handling.
 *
 * The encryption operation reverses the byte sequence of the input plain text,
 * producing a cipher text. Conversely, the decryption operation reverses the byte
 * sequence of the input cipher text, restoring the original plain text.
 */
object Reverse:
  given encrypt: Encrypt[Try] = (plainText: PlainText) =>
    Success(CipherText(plainText.bytes.reverse))
  given decrypt: Decrypt[Try] = (cipherText: CipherText) =>
    Success(PlainText(cipherText.bytes.reverse))
