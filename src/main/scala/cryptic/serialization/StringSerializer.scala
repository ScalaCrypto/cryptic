package cryptic
package serialization

import cryptic.PlainText
import cryptic.PlainText.{Deserializer, Serializer}

/**
  * Very inefficient, use a proper serializer.
  */
object StringSerializer {

  implicit val stringSerializer: Serializer[String] = (value: String) => PlainText(value)
  implicit val stringDeserializer: Deserializer[String] = (plainText: PlainText) => Right(new String(plainText).toString)
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

}
