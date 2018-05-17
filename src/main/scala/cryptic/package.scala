package object cryptic {
  type PlainText = Array[Byte]
  object PlainText {
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
  }
  type CipherText = Array[Byte]
  object CipherText {
    def apply(x: Array[Byte]): CipherText = x
  }
  trait Encrypt {
    def apply(plainText: PlainText): CipherText
  }
  trait Decrypt {
    def apply(cipherText: CipherText): Either[String, PlainText]
  }
}