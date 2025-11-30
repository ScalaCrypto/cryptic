package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.{Success, Try}

class EnigmaSpec
    extends AnyFlatSpec
    with Matchers
    with TryValues
    with TableDrivenPropertyChecks:
  behavior of "Enigma"

  import Enigma.default.{given, *}
  import Functor.tryFunctor

  val lorem: String = "lorem.txt".fromResource.glyph.string
  val loremCipherAAA_AAA: String = "lorem-cipher-AAA-AAA.txt".fromResource.glyph.string
  val loremCipherMAR_ADQ: String = "lorem-cipher-MAR_ADQ.txt".fromResource.glyph.string
  val loremCipherZBQ_AAA: String = "lorem-cipher-ZBQ-AAA.txt".fromResource.glyph.string

  "Enigma engine" should "encrypt and decrypt with explicit ring settings" in:
    val data = Table(
      ("plain", "cipher", "settings"),
      ("MARTIN", "WFSEXB", "III-II-I AAA AAZ B"),
      ("MARTIN", "SNJAJP", "III-II-I AAB AAZ B"),
      ("MARTIN", "HTFUSP", "III-II-I AAA AAA B"),
      ("MARTIN", "ITDVLW", "III-II-I AAE AAA B"),
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

  "Enigma" should "add random start position and encrypted message key to preamble" in:
    val secret = "Lorem ipsum dolor sit amet consectetur adipiscing elit sed do"
    val expected = secret.toUpperCase.filter(_.isLetter)
    given settings:String = "III-II-I AAA B"

    val encrypted: Encrypted[Try, String] = secret.encrypted
    encrypted.decrypted.success.value shouldBe expected
