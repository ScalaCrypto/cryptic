package cryptic
package cipher
package demo

import java.nio.ByteBuffer
import scala.util.{Failure, Success, Try}

/** Provides functionality for encryption and decryption operations based on the
  * Caesar cipher technique with an associated authentication data (AAD).
  *
  * This object defines a mechanism for handling keys, encryption, and
  * decryption while ensuring that key offsets are non-zero. The primary focus
  * is on integrating this logic with the `Try` effect type.
  */
object CaesarAAD:
  given functor: Functor[Try] = Functor.tryFunctor
  case class Keys(offsets: Map[String, Int]):
    require(offsets.forall(_._2 != 0), "Offsets cannot be zero")
    def get(keyId: String): Try[Int] =
      offsets
        .get(keyId)
        .toRight(new NoSuchElementException(s"Key ID $keyId not found"))
        .toTry

  object Keys:
    def apply(offsets: (String, Int)*): Keys = Keys(offsets.toMap)

  given encrypt(using keys: Keys): Encrypt[Try] =
    (plainText: PlainText) =>
      for
        offset <- keys.get(plainText.aad.string)
        encrypted <- plainText.bytes.addOffset(offset)
      yield CipherText(plainText.aad.bytes, encrypted)

  given decrypt(using keys: Keys): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(aadBytes, encrypted) =>
        val aad = AAD(aadBytes)
        val keyId = aad.string
        for
          offset <- keys.get(keyId)
          decrypted <- encrypted.addOffset(-offset)
        yield PlainText(decrypted, aad)
