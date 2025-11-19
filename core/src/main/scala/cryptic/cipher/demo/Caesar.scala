package cryptic
package cipher
package demo

import scala.util.{Failure, Success, Try}

/** Provides functionalities to perform Caesar cipher encryption and decryption
  * using a given key. The cipher shifts bytes by a specific offset defined by
  * the key.
  *
  * Encapsulation of operations is achieved using type classes for encryption
  * (`Encrypt`) and decryption (`Decrypt`), as well as functor support for
  * effectful computations.
  */
object Caesar:
  given functor: Functor[Try] = Functor.tryFunctor

  given encrypt(using key: Key): Encrypt[Try] = (plainText: PlainText) =>
    plainText.bytes.addOffset(key.offset).map(CipherText.apply)

  given decrypt(using key: Key): Decrypt[Try] = (cipherText: CipherText) =>
    cipherText.bytes.addOffset(-key.offset).map(PlainText.apply)
