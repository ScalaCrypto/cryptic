package cryptic
package crypto
package demo

import java.nio.ByteBuffer
import scala.util.Try

/**
 * The AADCaesar object provides methods and utilities for encryption and decryption
 * using an augmented Caesar cipher. This implementation incorporates an additional
 * authenticated associated data (AAD) parameter, which allows for different offsets
 * per key identifier in the map. It also supports the generation and management
 * of encryption keys.
 *
 * The cipher shifts each byte in the plaintext by a specified offset that corresponds
 * to a key identifier. Decryption reverses this operation using the same offset.
 *
 * Features:
 *
 * - Encryption and decryption facilitated by given instances of Encrypt and Decrypt
 * - Key management through the Keys class, allowing offsets to be associated with key IDs
 * - Validation of offsets to prevent empty maps and zero shifts
 * - Key generation with a specified key ID and offset
 */
object AADCaesar:
  case class Keys(offsets: Map[Int, Int]):
    require(offsets.nonEmpty, "Offsets map cannot be empty")
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
<<<<<<<< HEAD:core/src/main/scala/cryptic/crypto/demo/ManifestiCaesar.scala
  given encrypt(using keys: Keys): Encrypt[Try] = (plainText: PlainText) =>
    Try:
      val offset = keys.get(plainText.manifest.toKeyId)
      val bytes = plainText.bytes.mutable
        .map(b => (b + offset).toByte)
        .immutable
      CipherText(plainText.manifest, bytes)

  given decrypt(using keys: Keys): Decrypt[Try] = (cipherText: CipherText) =>
========
  given encrypt(using keys: Keys): Encrypt =
    (plainText: PlainText) =>
      val offset = keys.get(plainText.aad.toKeyId)
      val bytes = plainText.bytes.mutable
        .map(b => (b + offset).toByte)
        .immutable
      CipherText(plainText.aad, bytes)
  given decrypt(using keys: Keys): Decrypt = (cipherText: CipherText) =>
>>>>>>>> c8dc0a5 (removed relase, it's replaced by ci):core/src/main/scala/cryptic/crypto/demo/AADCaesar.scala
    Try:
      val IArray(aad, bytes) = cipherText.split
      val keyId = aad.toKeyId
      val offset = keys.get(keyId)
      val decoded = bytes.map(b => (b - offset).toByte)
      PlainText(decoded, aad)

  def keygen(keyId: Int, offset: Int): Keys = Keys(keyId -> offset)
extension (n: Int)
  def toAAD: IArray[Byte] =
    ByteBuffer.allocate(4).putInt(n).array().immutable
extension (bytes: IArray[Byte])
  def toKeyId: Int = ByteBuffer.wrap(bytes.mutable).getInt
