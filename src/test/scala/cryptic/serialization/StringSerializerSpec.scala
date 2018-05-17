package cryptic
package serialization

import org.scalatest._

class StringSerializerSpec extends FlatSpec with Matchers {
  import StringSerializer._

  "String serializer" should "encode to equal" in {
    stringSerializer.serialize("kalle") shouldEqual PlainText("kalle")
  }
  "String deserializer" should "decode to equal" in {
    stringDeserializer.deserialize(PlainText("kalle")) shouldEqual Right("kalle")
  }


}
