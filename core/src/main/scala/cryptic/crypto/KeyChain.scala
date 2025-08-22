package cryptic
package crypto

/** The KeyChain crypto provides a way of managing cryptographic keys for a
  * multitude of cyptos (using different or identical underlying cryptos for
  * different keys).
  */
case class KeyChain(
    keys: Map[KeyChain.Id, ?]
):
  require(keys.nonEmpty, "Key chain must contain at least one key")

  def add[T](id: KeyChain.Id, key: T): KeyChain =
    copy(keys = keys + (id -> key))

  def get[T](id: KeyChain.Id): T =
    keys
      .getOrElse(
        id,
        throw new NoSuchElementException(s"Key ID $id not found")
      )
      .asInstanceOf[T]

  override def toString: String = s"KeyChain(${keys.keys.mkString(", ")})"

object KeyChain:
  type Id = Int
  /*given encrypt(using keys: KeyChain): Encrypt =
    (plainText: PlainText) =>
      val offset = keys.get(plainText.manifest.toKey)
      val bytes = plainText.bytes.mutable
        .map(b => (b + offset).toByte)
        .immutable
      CipherText(plainText.manifest, bytes)
  given decrypt(using keys: Keys): Decrypt = (cipherText: CipherText) =>
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
    def toKey: Int = ByteBuffer.wrap(bytes.mutable).getInt
   */
