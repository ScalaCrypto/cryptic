package cryptic
package cipher
package enigma

import scala.util.{Success, Try}

case class Settings(rotors: Rotors, reflector: Reflector)

object Settings:
  /** Convenience constructor parsing a full settings string including reflector.
    *
    * Expected format (whitespace tolerant):
    *   "names rings positions reflector"
    *
    * Examples:
    *   - "III-II-I AAA AAZ B"
    *   - "VI-II-I ABC DEF C"
    *
    * Notes:
    *   - Rotor names are hyphen-separated, left-most to right-most.
    *   - Ring and position sequences are letters (A–Z or a–z), also left-most to right-most.
    *   - Reflector is mandatory and must be one of A, B, C (case-insensitive allowed via enum valueOf with uppercasing).
    */
  def apply(settings: String): Settings =
    // Match four parts: names, rings, positions, reflector
    val SettingsRe = """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s*$""".r

    settings match
      case SettingsRe(namesPart, ringsPart, posPart, reflPart) =>
        // Build Rotors from the first three parts using existing parser
        val rotors = Rotors(s"$namesPart $ringsPart $posPart")
        // Reflector must be a single letter A/B/C; tolerate full word matching enum name as well
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        Settings(rotors, reflector)
      case _ =>
        throw IllegalArgumentException(
          "Settings.apply requires format \"names rings positions reflector\" (e.g. \"III-II-I AAA AAZ B\")"
        )

object Enigma:

  object default:
    // Expose common givens and types when using this cipher's default package
    export cryptic.default.{given, *}
    export Enigma.{given, *}

  given encrypt(using settings: Settings): Encrypt[Try] =
    (plaintext: PlainText) => Success(CipherText(run(plaintext.bytes)))

  given decrypt(using settings: Settings): Decrypt[Try] =
    (cipherText: CipherText) => Success(PlainText(run(cipherText.bytes)))

  def run(bytes: IArray[Byte])(using settings: Settings): IArray[Byte] =
    var rotors = settings.rotors
    bytes
      .map(_.toChar)
      .filter(_.isGlyph)
      .map: c =>
        val g = c.glyph
        // Rotate all rotors applying ripple-carry rules
        rotors = rotors.rotate
        // Forward through rotors, reflect, then backward out
        val (forward, inTrace) = rotors.in(g)
        val reflected = settings.reflector.reflect(forward)
        val (output, outTrace) = rotors.out(reflected)
        scribe.debug(s"encoded ${inTrace.string}-${outTrace.string}")
        output.char.toByte
