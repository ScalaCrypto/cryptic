package cryptic
package serialization

import scala.util.Try

/** Very inefficient, use a proper serializer.
  */
object StringSerializer:
  implicit val stringSerializer: Serializer[String] = new Serializer[String]:
    override def serialize(value: String): PlainText = PlainText(value)
    override def deserialize(plainText: PlainText): Try[String] = Try(
      new String(plainText)
    )
  implicit val intSerializer: Serializer[Int] = new Serializer[Int]:
    override def serialize(value: Int): PlainText = PlainText(value.toString)
    override def deserialize(plainText: PlainText): Try[Int] = Try(
      new String(plainText).toInt
    )
  implicit val doubleSerializer: Serializer[Double] = new Serializer[Double]:
    override def serialize(value: Double): PlainText = PlainText(value.toString)
    override def deserialize(plainText: PlainText): Try[Double] = Try(
      new String(plainText).toDouble
    )
  implicit val booleanSerializer: Serializer[Boolean] =
    new Serializer[Boolean]:
      override def serialize(value: Boolean): PlainText = PlainText(
        value.toString
      )
      override def deserialize(plainText: PlainText): Try[Boolean] = Try(
        new String(plainText).toBoolean
      )
