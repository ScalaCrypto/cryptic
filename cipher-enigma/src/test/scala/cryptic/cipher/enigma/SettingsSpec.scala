package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

class SettingsSpec extends AnyFlatSpec with Matchers with TryValues:
  behavior of "Settings"
  "Settings" should "parse with start positions" in:
    Settings.parse("III-II-I ABC DEF C ABTD").success.value shouldBe Settings(
      Rotors("III-II-I ABC DEF"),
      Reflector.C,
      PlugBoard("ABTD"),
      false
    )
  "Settings" should "parse with without start positions" in:
    Settings.parse("III-II-I ABC C ABTD").success.value shouldBe Settings(
      Rotors("III-II-I ABC AAA"),
      Reflector.C,
      PlugBoard("ABTD"),
      true
    )
  "Settings" should "parse with start positions without plugboard" in:
    Settings.parse("III-II-I ABC DEF C").success.value shouldBe Settings(
      Rotors("III-II-I ABC DEF"),
      Reflector.C,
      PlugBoard.empty,
      false
    )
  "Settings" should "parse with with neither start positions nor plugboard" in:
    Settings.parse("III-II-I ABC C").success.value shouldBe Settings(
      Rotors("III-II-I ABC AAA"),
      Reflector.C,
      PlugBoard.empty,
      true
    )
