import java.nio.ByteBuffer
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

package object cryptic:
  type PlainText = Array[Byte]
  type Hash = Vector[Byte]
  object PlainText:
    val Empty: PlainText = PlainText(Array.emptyByteArray)
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
    def apply(x: Int): PlainText =
      val buffer = ByteBuffer.allocate(4)
      buffer.putInt(x)
      buffer.array()
    def apply(x: Long): PlainText =
      val buffer = ByteBuffer.allocate(8)
      buffer.putLong(x)
      buffer.array()
    def apply(x: Double): PlainText =
      val buffer = ByteBuffer.allocate(8)
      buffer.putDouble(x)
      buffer.array()
    def apply(x: Any): Nothing = throw new IllegalArgumentException(
      "Supported types are: Double, Int, Long and String"
    )
    def unapply[T](plainText: PlainText)(using ct: ClassTag[T]): Try[T] =
      def checkSize(n: Int): Try[PlainText] =
        if plainText.length == n then Success(plainText)
        else
          Failure(
            new IllegalArgumentException(
              s"PlainText must be exactly $n bytes for $ct extraction"
            )
          )
      ct.runtimeClass match
        case c if c == classOf[String] =>
          Try(new String(plainText).trim.asInstanceOf[T])
        case c if c == classOf[Int] =>
          checkSize(4).map(ByteBuffer.wrap(_).getInt.asInstanceOf[T])
        case c if c == classOf[Double] =>
          checkSize(8).map(ByteBuffer.wrap(_).getDouble.asInstanceOf[T])
        case c if c == classOf[Long] =>
          checkSize(8).map(ByteBuffer.wrap(_).getLong.asInstanceOf[T])
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
    digest.digest(plainText).toVector
  case class CipherText(bytes: Array[Byte]):
    override def equals(obj: scala.Any): Boolean = obj match
      case CipherText(other) ⇒ bytes.sameElements(other)
      case _ ⇒ false
    override def toString: String =
      s"${getClass.getCanonicalName.split('.').last}(0x${bytes.map("%02x".format(_)).mkString})"
  object CipherText:
    val Empty: CipherText = CipherText(Array.emptyByteArray)
  object Encrypt:
    val Empty: Encrypt = _ ⇒ CipherText.Empty
  trait Encrypt:
    def apply(plainText: PlainText): CipherText
  object Decrypt:
    val Empty: Decrypt = _ ⇒ Success(PlainText.Empty)
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
      plainText.array
    )
  given stringSerializer: Serializer[String] = new Serializer[String]:
    override def serialize(value: String): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[String] = Try(
      new String(plainText)
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
  def defaultSerializer[V: ClassTag]: Serializer[V] =
    new Serializer[V]:
      override def serialize(value: V): PlainText = PlainText(value)
      override def deserialize(plainText: PlainText): Try[V] =
        PlainText.unapply[V](plainText)

  extension [V: Serializer](value: V)
    def encrypted(using encrypt: Encrypt): Encrypted[V] = Encrypted(value)