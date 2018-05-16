package object cryptic {
  type PlainText = Array[Byte]
  object PlainText {
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
    trait Encoder[V] {
      def encode(value: V): PlainText
    }
    trait Decoder[V] {
      def decode(plainText: PlainText): Either[String, V]
    }
    implicit val nothingEncoder: Encoder[Nothing] = (value: Nothing) => throw new UnsupportedOperationException("encode empty")
    implicit val nothingDecoder: Decoder[Nothing] = (plainText: PlainText) => throw new UnsupportedOperationException("decode empty")
    implicit val stringEncoder: Encoder[String] = (value: String) => PlainText(value)
    implicit val stringDecoder: Decoder[String] = (plainText: PlainText) => Right(new String(plainText).toString)
    // Todo better serialization for Int, Double, Boolean
    implicit val intEncoder: Encoder[Int] = (value: Int) => PlainText(value.toString)
    implicit val intDecoder: Decoder[Int] = (plainText: PlainText) => Right(new String(plainText).toInt)
    implicit val doubleEncoder: Encoder[Double] = (value: Double) => PlainText(value.toString)
    implicit val doubleDecoder: Decoder[Double] = (plainText: PlainText) => Right(new String(plainText).toDouble)
    implicit val booleanEncoder: Encoder[Boolean] = (value: Boolean) => PlainText(value.toString)
    implicit val booleanDecoder: Decoder[Boolean] = (plainText: PlainText) => Right(new String (plainText).toBoolean)
  }
  type CipherText = Array[Byte]
  object CipherText {
    def apply(x: Array[Byte]): CipherText = x
  }
}