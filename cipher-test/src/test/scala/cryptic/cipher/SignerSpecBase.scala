package cryptic.cipher

import cryptic.*

import scala.compiletime.deferred
import scala.util.Try

trait SignerSpecBase extends SpecBase:

  given sign: Sign[Try] = deferred
  given verify: Verify[Try] = deferred

  given fooCodec: Codec[Foo] = deferred

  "Signer" should "sign and verify case class" in:
    val foo = Foo("text")
    val fooSigned = foo.signed
    fooSigned.text.success.value shouldBe foo
    fooSigned.verified.success.value shouldBe foo
