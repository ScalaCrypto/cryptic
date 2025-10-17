package cryptic
package crypto

import scala.concurrent.Future

/** NOTE This crypto is only for testing, use a proper algorithm for production!
  *
  * The `Caesar` object provides encryption and decryption functionality using
  * the Caesar cipher. It includes methods to generate keys, and given methods
  * to encrypt and decrypt text using a specified key.
  *
  * The Caesar cipher is a type of substitution cipher in which each letter in
  * the plaintext is shifted a certain number of places down or up the alphabet
  * based on the given offset.
  *
  * The `Key` case class ensures that the offset is non-zero.
  */
object FutureCaesar:
  case class Key(offset: Int):
    require(offset != 0)
  given encrypt(using key: Key): Encrypt[Future] =
    Encrypt.lift((plainText: PlainText) =>
      val bytes =
        plainText.bytes.mutable.map(b ⇒ (b + key.offset).toByte).immutable
      Future.successful(CipherText(bytes)))
  given decrypt(using key: Key): Decrypt[Future] = (cipherText: CipherText) =>
    Future.successful(
      PlainText(cipherText.bytes.map(b => (b - key.offset).toByte))
    )

  def keygen(offset: Int): Key = Key(offset)
