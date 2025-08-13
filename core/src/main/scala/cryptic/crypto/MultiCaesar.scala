package cryptic
package crypto

import java.nio.ByteBuffer
import scala.util.Try

/** NOTE This crypto is only for testing, use a proper algorithm
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
object MultiCaesar:
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
  given encrypt(using keys: Keys): Encrypt =
    (plainText: PlainText) =>
      val offset = keys.get(plainText.manifest.toKeyId)
      val bytes =
        plainText.bytes.mutable
          .map: b =>
            (b + offset).toByte
          .immutable
      CipherText(plainText.manifest, bytes)
  given decrypt(using keys: Keys): Decrypt = (cipherText: CipherText) =>
    val IArray(manifest, bytes) = cipherText.split
    Try[PlainText](
      PlainText(bytes.map(b => (b - keys.offsets(manifest.toKeyId)).toByte))
    )

  def keygen(keyId: Int, offset: Int): Keys = Keys(keyId -> offset)
extension (n: Int)
  def toManifest: IArray[Byte] =
    ByteBuffer.allocate(4).putInt(n).array().immutable
extension (bytes: IArray[Byte])
  def toKeyId: Int = ByteBuffer.wrap(bytes.mutable).getInt
