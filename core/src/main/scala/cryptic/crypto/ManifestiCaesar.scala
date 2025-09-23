package cryptic
package crypto

import java.nio.ByteBuffer
import scala.util.Try

/** NOTE This crypto is only for testing, use a proper algorithm for production!
  *
  * The `MultiCaesar` object extends on the Caesar cipher concept by allowing
  * multiple keys, each with a unique offset. The keys are provided when
  * encrypting the text and then encoded in the clear text manifest. When
  * decrypting, the correct key is retrieved based on the contents of the
  * manifest.
  */
object ManifestCaesar:
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
  given encrypt(using keys: Keys): Encrypt[Id] =
    Encrypt.fromFunction((plainText: PlainText) =>
      val offset = keys.get(plainText.manifest.toKeyId)
      val bytes = plainText.bytes.mutable
        .map(b => (b + offset).toByte)
        .immutable
      CipherText(plainText.manifest, bytes)
    )
  given decrypt(using keys: Keys): Decrypt[Try] = (cipherText: CipherText) =>
    Try:
      val IArray(manifest, bytes) = cipherText.split
      val keyId = manifest.toKeyId
      val offset = keys.get(keyId)
      val decoded = bytes.map(b => (b - offset).toByte)
      PlainText(decoded, manifest)

  def keygen(keyId: Int, offset: Int): Keys = Keys(keyId -> offset)
extension (n: Int)
  def toManifest: IArray[Byte] =
    ByteBuffer.allocate(4).putInt(n).array().immutable
extension (bytes: IArray[Byte])
  def toKeyId: Int = ByteBuffer.wrap(bytes.mutable).getInt
