package cryptic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues
import scala.util.Success

class FixedVersionSpec extends AnyFlatSpec with Matchers with TryValues:
  "FixedVersion" should "compare versions correctly" in:
    val v1 = FixedVersion(1, 0, 0, 0)
    val v2 = FixedVersion(1, 0, 0, 1)
    v1.compare(v2) should be < 0
    v2.compare(v1) should be > 0
    v1.compare(v1) should be(0)

  it should "convert to and from bytes" in:
    val version = FixedVersion(1, 2, 3, 4)
    val bytes = version.bytes
    FixedVersion(bytes) shouldBe version

  it should "validate version support" in:
    val version = FixedVersion(1, 0, 0, 0)
    version.supports(version.bytes) shouldBe true
    version.supports(FixedVersion(1, 0, 0, 1).bytes) shouldBe false

  it should "handle invalid version formats" in:
    intercept[IllegalArgumentException]:
      FixedVersion(1, 2, 3) // Invalid length
    intercept[IllegalArgumentException]:
      FixedVersion(1, 2, 3, 4, 5) // Invalid length

  it should "format toString correctly" in :
    val version = FixedVersion(1, 2, 3, 4)
    version.toString shouldBe "1.2.3.4"
