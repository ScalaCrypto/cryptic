package object cryptic {
  type PlainText = Array[Byte]
  object PlainText {
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
    trait Serializer[V] {
      def serialize(value: V): PlainText
    }
    trait Deserializer[V] {
      def deserialize(plainText: PlainText): Either[String, V]
    }
    implicit val nothingSerializer: Serializer[Nothing] = (value: Nothing) => throw new UnsupportedOperationException("encode empty")
    implicit val nothingDeserializer: Deserializer[Nothing] = (plainText: PlainText) => throw new UnsupportedOperationException("decode empty")
    implicit val stringSerializer: Serializer[String] = (value: String) => PlainText(value)
    implicit val stringDeserializer: Deserializer[String] = (plainText: PlainText) => Right(new String(plainText).toString)
    // Todo better serialization for Int, Double, Boolean
    implicit val intSerializer: Serializer[Int] = (value: Int) =>
      PlainText(value.toString)
    implicit val intDeserializer: Deserializer[Int] = (plainText: PlainText) =>
      Right(new String(plainText).toInt)
    implicit val doubleSerializer: Serializer[Double] = (value: Double) =>
      PlainText(value.toString)
    implicit val doubleDeserializer: Deserializer[Double] = (plainText: PlainText) =>
      Right(new String(plainText).toDouble)
    implicit val booleanSerializer: Serializer[Boolean] = (value: Boolean) =>
      PlainText(value.toString)
    implicit val booleanDeserializer: Deserializer[Boolean] = (plainText: PlainText) =>
      Right(new String (plainText).toBoolean)
    implicit def optionSerializer[V : Serializer]: Serializer[Option[V]] = {
      case Some(v: V) => implicitly[Serializer[V]].serialize(v)
      case None => PlainText("")
    }
    implicit def optionDeserializer[V : Deserializer]: Deserializer[Option[V]] = (plainText: PlainText) => 
      implicitly[Deserializer[V]].deserialize(plainText).map(Option.apply)
  }
  type CipherText = Array[Byte]
  object CipherText {
    def apply(x: Array[Byte]): CipherText = x
  }
}