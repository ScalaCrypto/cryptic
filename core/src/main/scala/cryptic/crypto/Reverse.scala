package cryptic
package crypto

import scala.util.{Success, Try}

/** The Reverse object provides given values for encryption and decryption by
  * reversing the bytes.
  *
  * It defines two given values:
  *   - `encrypt`: Given Encrypt instance that reverses the bytes of the
  *     input PlainText.
  *   - `decrypt`: Given Decrypt instance that reverses the bytes of the
  *     input CipherText.
  *
  * The encryption and decryption process involves simply reversing the byte
  * sequence.
  */
object Reverse:
  given encrypt: Encrypt[Id] = Encrypt.fromFunction((plainText: PlainText) =>
    CipherText(plainText.bytes.reverse)
  )
  given decrypt: Decrypt[Try] = (cipherText: CipherText) =>
    Success(PlainText(cipherText.bytes.reverse))
