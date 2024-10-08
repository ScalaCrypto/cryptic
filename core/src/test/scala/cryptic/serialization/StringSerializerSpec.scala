package cryptic
package serialization

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class StringSerializerSpec extends AnyFlatSpec with Matchers {
  import StringSerializer._

  "String serializer" should "encode to equal" in {
    stringSerializer.serialize("kalle") shouldEqual PlainText("kalle")
  }
  "String deserializer" should "decode to equal" in {
    stringSerializer.deserialize(PlainText("kalle")) shouldEqual Success("kalle")
  }

}
