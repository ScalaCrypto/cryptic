package object cryptic {
  type PlainText = Array[Byte]
  type Hash = Vector[Byte]
  object PlainText {
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
  trait Encrypt {
    def apply(plainText: PlainText): CipherText
  }
  trait Decrypt {
    def apply(cipherText: CipherText): Either[String, PlainText]
  }
}