package object cryptic {
  type PlainText = Array[Byte]
  type Hash = Vector[Byte]
  object PlainText {
    val Empty = PlainText(Array.emptyByteArray)
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
  }
  def hash(plainText: PlainText): Hash = {
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(plainText).toVector
  }
  case class CipherText(hash: Hash)(val bytes: Array[Byte])
  object CipherText {
    val Empty: CipherText = CipherText(Vector.empty)(Array.emptyByteArray)
  }
  object Encrypt {
    val Empty: Encrypt = _ ⇒ CipherText.Empty
  }
  trait Encrypt {
    def apply(plainText: PlainText): CipherText
  }
  object Decrypt {
    val Empty: Decrypt = _ ⇒ Right(PlainText.Empty)
  }
  trait Decrypt {
    def apply(cipherText: CipherText): Either[String, PlainText]
  }
}