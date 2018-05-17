package object cryptic {
  type PlainText = Array[Byte]
  object PlainText {
    def apply(x: Array[Byte]): PlainText = x
    def apply(x: String): PlainText = x.getBytes()
    trait Serializer[V] {
      def serialize(value: V): PlainText
    }
    object Serializer {
    implicit val nothingSerializer: Serializer[Nothing] = (value: Nothing) => throw new UnsupportedOperationException("encode empty")
    }
    trait Deserializer[V] {
      def deserialize(plainText: PlainText): Either[String, V]
    }
    object Deserializer {
    implicit val nothingDeserializer: Deserializer[Nothing] = (plainText: PlainText) => throw new UnsupportedOperationException("decode empty")
    }
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
  trait Encrypt {
    def apply(plainText: PlainText): CipherText
  }
  trait Decrypt {
    def apply(cipherText: CipherText): Either[String, PlainText]
  }
}