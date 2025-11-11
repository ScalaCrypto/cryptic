package cryptic
package cipher
package demo

import java.nio.ByteBuffer
import scala.util.Try

/**
 * The `CaesarAAD` object provides encryption and decryption functionality using a Caesar cipher
 * with an additional "Associated Authentication Data" (AAD) mechanism for key identification.
 * It defines methods and given instances for the `Encrypt` and `Decrypt` type classes, as well as utilities
 * for key management.
 *
 * The AAD is used as a metadata segment to facilitate dynamic key-based encryption and decryption.
 * Each AAD is associated with a key ID, allowing multiple keys to be used simultaneously.
 *
 * The `Keys` case class encapsulates a mapping between `keyId` and their offsets, enabling flexible
 * encryption for various key IDs.
 */
object CaesarAAD:
  case class Keys(offsets: Map[Int, Int]):
    require(offsets.nonEmpty, "Offsets cannot be empty")
    require(offsets.forall(_._2 != 0), "Offsets cannot be zero")
    def add(keyId: Int, offset: Int): Keys =
      copy(offsets = offsets + (keyId -> offset))
    def get(keyId: Int): Int =
      offsets.getOrElse(
        keyId,
        throw new NoSuchElementException(s"Key ID $keyId not found")
      )
  object Keys:
    def apply(offsets: (Int, Int)*): Keys =
      Keys(offsets.toMap)
  given encrypt(using keys: Keys): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val offset = keys.get(plainText.aad.toKeyId)
        val bytes = plainText.bytes.mutable
          .map(b => (b + offset).toByte)
          .immutable
        CipherText(plainText.aad, bytes)
  given decrypt(using keys: Keys): Decrypt[Try] = (cipherText: CipherText) =>
    Try:
      val IArray(aad, bytes) = cipherText.split
      val keyId = aad.toKeyId
      val offset = keys.get(keyId)
      val decoded = bytes.map(b => (b - offset).toByte)
      PlainText(decoded, aad)

  def keygen(keyId: Int, offset: Int): Keys = Keys(keyId -> offset)
extension (n: Int)
  def toAAD: AAD =
    ByteBuffer.allocate(4).putInt(n).array().aad
extension (bytes: IArray[Byte])
  def toKeyId: Int = ByteBuffer.wrap(bytes.mutable).getInt
