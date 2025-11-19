package cryptic
package cipher
package demo

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

/** Provides operations for Caesar cipher encryption and decryption using
  * `Future` as an effect type. Includes given instances for `Functor` and
  * implementations of encryption and decryption.
  */
object CaesarFuture:
  given functor: Functor[Future] = Functor.futureFunctor

  given encrypt(using key: Key, ec: ExecutionContext): Encrypt[Future] =
    (plainText: PlainText) =>
      plainText.bytes.addOffset(key.offset).map(CipherText.apply)

  given decrypt(using key: Key, ec: ExecutionContext): Decrypt[Future] =
    (cipherText: CipherText) =>
      cipherText.bytes.addOffset(-key.offset).map(PlainText.apply)
