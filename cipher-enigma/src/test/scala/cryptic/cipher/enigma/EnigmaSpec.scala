package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Try

class EnigmaSpec
    extends AnyFlatSpec
    with Matchers
    with TryValues
    with TableDrivenPropertyChecks:
  behavior of "Enigma"

  import Enigma.default.{*, given}

  val lorem: String = "lorem.txt".fromResource.glyph.string
  val loremCipherAAA_AAA: String =
    "lorem-cipher-AAA-AAA.txt".fromResource.glyph.string
  val loremCipherMAR_ADQ: String =
    "lorem-cipher-MAR_ADQ.txt".fromResource.glyph.string
  val loremCipherZBQ_AAA: String =
    "lorem-cipher-ZBQ-AAA.txt".fromResource.glyph.string

  "Enigma engine" should "encrypt and decrypt with explicit ring settings" in:
    val data = Table(
      ("plain", "cipher", "settings"),
      ("MARTIN", "WFSEXB", "III-II-I AAA AAZ B"),
      ("MARTIN", "SNJAJP", "III-II-I AAB AAZ B"),
      ("MARTIN", "HTFUSP", "III-II-I AAA AAA B"),
      ("MARTIN", "ITDVLW", "III-II-I AAE AAA B"),
      ("MARTIN", "FDEQBM", "III-II-I AAA AAA B MARTIN"),
      ("ABCD", "ZUIN", "III-II-I AAA ADQ B"),
      ("ABCD", "NODJ", "III-II-I AAA LEQ B"),
      (lorem, loremCipherAAA_AAA, "III-II-I AAA AAA B"),
      (lorem, loremCipherMAR_ADQ, "III-II-I MAR ADQ B"),
      (lorem, loremCipherZBQ_AAA, "III-II-I ZBQ AAA B")
    )
    forAll(data)((plain: String, cipher: String, settings: String) =>
      given s: Settings = Settings(settings)
      Enigma.run(plain.bytes).string shouldBe cipher
    )

  "Settings" should "parse with pos settings" in:
    val settings = Settings("III-II-I AAA AAZ B")
    settings.rotors.rotors shouldBe Rotors("III-II-I AAA AAZ ").rotors
    settings.reflector shouldBe Reflector.B

  "Settings" should "parse without pos settings default to AAA" in:
    val settings = Settings("III-II-I AAZ B")
    settings.rotors.rotors shouldBe Rotors("III-II-I AAZ AAA ").rotors
    settings.reflector shouldBe Reflector.B

  "Settings" should "parse plugboard as last token when provided" in:
    val settings = Settings("III-II-I AAA AAZ B ABCD")
    settings.rotors.rotors shouldBe Rotors("III-II-I AAA AAZ ").rotors
    settings.reflector shouldBe Reflector.B
    // Plugboard ABCD => A<->B, C<->D
    settings.plugboard.swap('A'.glyph).char shouldBe 'B'
    settings.plugboard.swap('B'.glyph).char shouldBe 'A'
    settings.plugboard.swap('C'.glyph).char shouldBe 'D'
    settings.plugboard.swap('E'.glyph).char shouldBe 'E'

  "Enigma" should "add random start position and encrypted message key to preamble" in:
    val plain =
      "Lorem ipsum dolor sit amet consectetur adipiscing elit sed do".glyph.string
    given settings: Settings = Settings("III-II-I AAA B")
    val encrypted: Encrypted[Try, String] = plain.encrypted
    encrypted.decrypted.success.value shouldBe plain

  "Enigma" should "apply plugboard swap before and after rotors" in:
    val base = Settings("III-II-I AAA AAZ B")
    val withPB = Settings("III-II-I AAA AAZ B QW") // Q<->W pair
    val plug = withPB.plugboard
    val msg = "HELLOWORLD"

    // Encrypt with plugboard
    val outWithPB = Enigma.run(msg.glyph)(using withPB)

    // Encrypt without plugboard but manually swap before and after
    val preSwapped = msg.glyph.map(plug.swap)
    val core = Enigma.run(preSwapped)(using base)
    val postSwapped = core.map(plug.swap)

    outWithPB shouldBe postSwapped
