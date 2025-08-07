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
    def apply(x: Float): PlainText =
      val buffer = ByteBuffer.allocate(4)
      buffer.putFloat(x)
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
        case c if c == classOf[Float] =>
          checkSize(4).map(plainText =>
            ByteBuffer.wrap(plainText.bytes).getFloat.asInstanceOf[T]
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
  trait Encrypt:
    def apply(plainText: PlainText): CipherText
  object Encrypt:
    val Empty: Encrypt = _ ⇒ CipherText.Empty
  trait Decrypt:
    def apply(cipherText: CipherText): Try[PlainText]
  object Decrypt:
    val Empty: Decrypt = _ ⇒ Success(PlainText.empty)
  trait Codec[V]:
    def encode(value: V): PlainText
    def decode(plainText: PlainText): Try[V]
  object Codec:
    given Codec[Nothing] = new Codec[Nothing]:
      override def encode(value: Nothing): PlainText =
        throw new UnsupportedOperationException("encode nothing")
      override def decode(plainText: PlainText): Try[Nothing] = Failure(
        new UnsupportedOperationException("decode nothing")
      )
    given Codec[Array[Byte]] = new Codec[Array[Byte]]:
      override def encode(value: Array[Byte]): PlainText = PlainText(value)
      override def decode(plainText: PlainText): Try[Array[Byte]] = Try(
        plainText.bytes
      )
    given Codec[String] = new Codec[String]:
      override def encode(value: String): PlainText = PlainText(value)
      override def decode(plainText: PlainText): Try[String] = Try(
        new String(plainText.bytes)
      )
    given Codec[Int] = new Codec[Int]:
      override def encode(value: Int): PlainText = PlainText(value)
      override def decode(plainText: PlainText): Try[Int] =
        PlainText.unapply[Int](plainText)
    given Codec[Long] = new Codec[Long]:
      override def encode(value: Long): PlainText = PlainText(value)
      override def decode(plainText: PlainText): Try[Long] =
        PlainText.unapply[Long](plainText)
    given Codec[Float] = new Codec[Float]:
      override def encode(value: Float): PlainText = PlainText(value)
      override def decode(plainText: PlainText): Try[Float] =
        PlainText.unapply[Float](plainText)
    given Codec[Double] = new Codec[Double]:
      override def encode(value: Double): PlainText = PlainText(value)
      override def decode(plainText: PlainText): Try[Double] =
        PlainText.unapply[Double](plainText)

  extension [V: Codec](value: V)
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
