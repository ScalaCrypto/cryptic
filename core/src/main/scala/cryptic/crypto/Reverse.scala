package cryptic
package crypto

import scala.util.Success

/** The Reverse object provides implicit values for encryption and decryption by
  * reversing the bytes.
  *
  * It defines two implicit values:
  *   - `encrypt`: Implicit Encrypt instance that reverses the bytes of the
  *     input PlainText.
  *   - `decrypt`: Implicit Decrypt instance that reverses the bytes of the
  *     input CipherText.
  *
  * The encryption and decryption process involves simply reversing the byte
  * sequence.
  */
object Reverse:
  given encrypt: Encrypt = (plainText: PlainText) =>
    CipherText(plainText.reverse)
  given decrypt: Decrypt = (cipherText: CipherText) =>
    Success(PlainText(cipherText.bytes.reverse))
