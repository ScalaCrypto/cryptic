package cryptic
package crypto
package demo

import scala.util.{Failure, Success, Try}

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
