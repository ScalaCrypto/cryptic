package object cryptic {
  type PlainText = String
  object PlainText {
    def apply(x: Any): PlainText = String.valueOf(x)
    trait Encoder[V] {
      def encode(value: V): PlainText
    }
    trait Decoder[V] {
      def decode(plainText: PlainText): Either[String, V]
    }
    implicit val nothingEncoder: Encoder[Nothing] = (value: Nothing) => throw new UnsupportedOperationException("encode empty")
    implicit val nothingDecoder: Decoder[Nothing] = (plainText: PlainText) => throw new UnsupportedOperationException("decode empty")
    implicit val stringEncoder: Encoder[String] = (value: String) => PlainText(value)
    implicit val stringDecoder: Decoder[String] = (plainText: PlainText) => Right(plainText)
    implicit val intEncoder: Encoder[Int] = (value: Int) => PlainText(value.toString)
    implicit val intDecoder: Decoder[Int] = (plainText: PlainText) => Right(plainText.toInt)
    implicit val doubleEncoder: Encoder[Double] = (value: Double) => PlainText(value.toString)
    implicit val doubleDecoder: Decoder[Double] = (plainText: PlainText) => Right(plainText.toDouble)
    implicit val booleanEncoder: Encoder[Boolean] = (value: Boolean) => PlainText(value.toString)
    implicit val booleanDecoder: Decoder[Boolean] = (plainText: PlainText) => Right(plainText.toBoolean)
  }
  type CipherText = String
  object CipherText {
    def apply(x: Any): CipherText = String.valueOf(x)
  }
}