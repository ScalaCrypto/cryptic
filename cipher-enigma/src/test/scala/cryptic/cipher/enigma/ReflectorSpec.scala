package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReflectorSpec extends AnyFlatSpec with Matchers:
  behavior of "Reflector"

  private val a: Reflector = Reflector.A
  private val b: Reflector = Reflector.B
  private val c: Reflector = Reflector.C
  "Reflectors" should "be symmetric" in:
    ('A' to 'Z').foreach: i =>
      a.reflect(a.reflect(i)) shouldBe i
      b.reflect(b.reflect(i)) shouldBe i
      c.reflect(c.reflect(i)) shouldBe i

  "Reflector B" should "map" in:
    ('A' to 'Z').foreach: c =>
      b.reflect(c) should not be c
