import scala.util.{ Success, Try }

package object cryptic {
  type PlainText = Array[Byte]
  type Hash = Vector[Byte]
  object PlainText {
    val Empty: PlainText = PlainText(Array.emptyByteArray)
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
  }
  def hash(plainText: PlainText): Hash = {
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(plainText).toVector
  }
  case class CipherText(bytes: Array[Byte]) {
    override def equals(obj: scala.Any): Boolean = obj match {
      case CipherText(other) ⇒ bytes.sameElements(other)
      case _ ⇒ false
    }
    override def toString: String =
      s"${getClass.getCanonicalName.split('.').last}(0x${bytes.map("%02x".format(_)).mkString})"
  }
  object CipherText {
    val Empty: CipherText = CipherText(Array.emptyByteArray)
  }
  object Encrypt {
    val Empty: Encrypt = _ ⇒ CipherText.Empty
  }
  trait Encrypt {
    def apply(plainText: PlainText): CipherText
  }
  object Decrypt {
    val Empty: Decrypt = _ ⇒ Success(PlainText.Empty)
  }
  trait Decrypt {
    def apply(cipherText: CipherText): Try[PlainText]
  }
}
