import java.nio.ByteBuffer
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

package object cryptic:
  type Hash = Vector[Byte]
  type Manifest = Array[Byte]
  object Manifest:
    val empty: Array[Byte] = Array.emptyByteArray
  case class PlainText(bytes: Array[Byte], manifest: Manifest = Manifest.empty):
//    override def toString: String = "\uD83D\uDD12"

    override def equals(obj: Any): Boolean = obj match
      case other: PlainText =>
        manifest.sameElements(other.manifest) && bytes.sameElements(other.bytes)
      case _ => false

    override def hashCode: Int =
      java.util.Arrays.hashCode(bytes) * 31 + java.util.Arrays.hashCode(
        manifest
      )

  object PlainText:
    val empty: PlainText = PlainText(Array.emptyByteArray)
    def apply(x: Array[Byte]): PlainText = new PlainText(x)
    def apply(x: String): PlainText = apply(x.getBytes())
    def apply(x: Int): PlainText =
      val buffer = ByteBuffer.allocate(4)
      buffer.putInt(x)
      apply(buffer.array())

    def apply(x: Long): PlainText =
      val buffer = ByteBuffer.allocate(8)
      buffer.putLong(x)
      apply(buffer.array())
    def apply(x: Double): PlainText =
      val buffer = ByteBuffer.allocate(8)
      buffer.putDouble(x)
      apply(buffer.array())
    def unapply[T](plainText: PlainText)(using ct: ClassTag[T]): Try[T] =
      def checkSize(n: Int): Try[PlainText] =
        if plainText.bytes.length == n then Success(plainText)
        else
          Failure(
            new IllegalArgumentException(
              s"PlainText must be exactly $n bytes for $ct extraction"
            )
          )
      ct.runtimeClass match
        case c if c == classOf[String] =>
          Try(new String(plainText.bytes).trim.asInstanceOf[T])
        case c if c == classOf[Int] =>
          checkSize(4).map(plainText =>
            ByteBuffer.wrap(plainText.bytes).getInt.asInstanceOf[T]
          )
        case c if c == classOf[Double] =>
          checkSize(8).map(plainText =>
            ByteBuffer.wrap(plainText.bytes).getDouble.asInstanceOf[T]
          )
        case c if c == classOf[Long] =>
          checkSize(8).map(plainText =>
            ByteBuffer.wrap(plainText.bytes).getLong.asInstanceOf[T]
          )
        case _ =>
          Failure(
            new IllegalArgumentException(
              s"Unsupported type $ct for extraction. " +
                "Supported types are: Double, Int, Long and String"
            )
          )

  def hash(plainText: PlainText): Hash =
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(plainText.bytes).toVector // Todo handle manifest?
  case class CipherText(bytes: Array[Byte]):
    def buffer: ByteBuffer = ByteBuffer.wrap(bytes)
    def split: Array[Array[Byte]] = buffer.split
    override def equals(obj: scala.Any): Boolean = obj match
      case CipherText(other) ⇒ bytes.sameElements(other)
      case _ ⇒ false
    override def toString: String =
      s"${getClass.getCanonicalName.split('.').last}(0x${bytes.map("%02x".format(_)).mkString})"
  object CipherText:
    val Empty: CipherText = CipherText(Array.emptyByteArray)
    def apply(array: Array[Byte], arrays: Array[Byte]*): CipherText =
      val count = 1 + arrays.length
      val buffer = ByteBuffer.allocate(4 + count * 4 + arrays.foldLeft(0):
        case (length, bytes) => length + bytes.length)
      buffer.putInt(count)
      buffer.nextBytes(array)
      arrays.foldLeft(buffer):
        case (buffer, bytes) => buffer.nextBytes(bytes)
      new CipherText(buffer.array())
    def unapplySeq(cipherText: CipherText): Option[Seq[Array[Byte]]] =
      Option(cipherText.split)
  object Encrypt:
    val Empty: Encrypt = _ ⇒ CipherText.Empty
  trait Encrypt:
    def apply(plainText: PlainText): CipherText
  object Decrypt:
    val Empty: Decrypt = _ ⇒ Success(PlainText.empty)
  trait Decrypt:
    def apply(cipherText: CipherText): Try[PlainText]
  trait Serializer[V]:
    def serialize(value: V): PlainText
    def deserialize(plainText: PlainText): Try[V]
  given nothingSerializer: Serializer[Nothing] = new Serializer[Nothing]:
    override def serialize(value: Nothing): PlainText =
      throw new UnsupportedOperationException("serialize nothing")
    override def deserialize(plainText: PlainText): Try[Nothing] = Failure(
      new UnsupportedOperationException("deserialize nothing")
    )
  given bytesSerializer: Serializer[Array[Byte]] = new Serializer[Array[Byte]]:
    override def serialize(value: Array[Byte]): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[Array[Byte]] = Try(
      plainText.bytes
    )
  given stringSerializer: Serializer[String] = new Serializer[String]:
    override def serialize(value: String): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[String] = Try(
      new String(plainText.bytes)
    )
  given intSerializer: Serializer[Int] = new Serializer[Int]:
    override def serialize(value: Int): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[Int] =
      PlainText.unapply[Int](plainText)
  given longSerializer: Serializer[Long] = new Serializer[Long]:
    override def serialize(value: Long): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[Long] =
      PlainText.unapply[Long](plainText)
  given doubleSerializer: Serializer[Double] = new Serializer[Double]:
    override def serialize(value: Double): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[Double] =
      PlainText.unapply[Double](plainText)

  extension [V: Serializer](value: V)
    def encrypted(using encrypt: Encrypt): Encrypted[V] = Encrypted(value)

  extension (buffer: ByteBuffer)
    def nextBytes(): Array[Byte] =
      val length = buffer.getInt()
      val bytes =
        if length == 0 then Array.emptyByteArray else new Array[Byte](length)
      buffer.get(bytes)
      bytes
    def nextBytes(bytes: Array[Byte]): ByteBuffer =
      buffer.putInt(bytes.length)
      buffer.put(bytes)
    def split: Array[Array[Byte]] =
      val count = buffer.getInt()
      val arrays = Array.ofDim[Array[Byte]](count)
      for i <- 0 until count do arrays(i) = buffer.nextBytes()
      arrays
