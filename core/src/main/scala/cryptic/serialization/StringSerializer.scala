package cryptic
package serialization

/** Very inefficient, use a proper serializer.
  */
object StringSerializer {
  implicit val stringSerializer: Serializer[String] = new Serializer[String] {
    override def serialize(value: String) = PlainText(value)
    override def deserialize(plainText: PlainText) = Right(
      new String(plainText).toString
    )
  }
  implicit val intSerializer: Serializer[Int] = new Serializer[Int] {
    override def serialize(value: Int) = PlainText(value.toString)
    override def deserialize(plainText: PlainText) = Right(
      new String(plainText).toInt
    )
  }
  implicit val doubleSerializer: Serializer[Double] = new Serializer[Double] {
    override def serialize(value: Double) = PlainText(value.toString)
    override def deserialize(plainText: PlainText) = Right(
      new String(plainText).toDouble
    )
  }
  implicit val booleanSerializer: Serializer[Boolean] =
    new Serializer[Boolean] {
      override def serialize(value: Boolean) = PlainText(value.toString)
      override def deserialize(plainText: PlainText) = Right(
        new String(plainText).toBoolean
      )
    }
}
