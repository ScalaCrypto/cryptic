package cryptic
package serialization

import org.scalatest._

class FstSerializerSpec extends FlatSpec with Matchers {
  import FstSerializer._

  "Fst serializer" should "serialize then deserialize back to original" in {
    val pt = serializer.serialize("kalle")
    pt shouldNot equal(PlainText("kalle"))
    val o = serializer.deserialize(pt)
    o shouldEqual Right("kalle")
  }
}
