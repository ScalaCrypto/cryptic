package cryptic
package cipher
package enigma

import scala.util.{Success, Try}

case class Settings(rotors: Rotors, reflector: Reflector):
  /**
   * Adjusts the rotor positions using the given array of glyphs and returns an updated Settings object.
   *
   * @param pos An immutable array of Glyph objects representing the rotor positions to be set.
   * @return A new Settings instance with the updated rotor positions.
   */
  def pos(pos: IArray[Glyph]): Settings = copy(rotors = rotors.pos(pos))

object Settings:
  /** Parses a settings string and constructs a `Settings` instance representing
    * the rotor and reflector configurations for an Enigma machine.
    *
    * The input settings string must match one of the following formats:
    *   - `"names rings positions reflector"` (e.g., `"III-II-I AAA AAZ B"`)
    *   - `"names rings reflector"` (e.g., `"III-II-I AAA B"`), in which case
    *     positions will default to all 'A'.
    *
    * @param settings
    *   the input string specifying the rotor names, ring settings, (optional)
    *   initial positions, and reflector. Valid formats:
    *   `"names rings positions reflector"` or `"names rings reflector"`.
    *   - `names` are hyphen-separated rotor IDs (left-to-right order).
    *   - `rings` and `positions` are sequences of letters where each length
    *     matches the number of rotor names.
    *   - `reflector` is a single letter (e.g., `B`).
    *
    * Example: "III-II-I AAA AAZ B" or "III-II-I AAA B".
    * @return
    *   a `Settings` object representing the parsed rotor and reflector
    *   configurations.
    * @throws IllegalArgumentException
    *   if the input does not adhere to the expected format, if the lengths of
    *   `names`, `rings`, or `positions` are mismatched, or if any values are
    *   invalid (e.g., unknown rotor names, invalid reflector).
    */
  def apply(settings: String): Settings =
    // Match four parts: names, rings, positions, reflector
    val WithPos =
      """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s*$""".r
    val NoPos = """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s*$""".r

    settings match
      case WithPos(namesPart, ringsPart, posPart, reflPart) =>
        val rotors = Rotors(s"$namesPart $ringsPart $posPart")
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        Settings(rotors, reflector)
      case NoPos(namesPart, ringPart, reflPart) =>
        val rotors = Rotors(s"$namesPart $ringPart ${"A" * ringPart.length}")
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        Settings(rotors, reflector)
      case _ =>
        throw IllegalArgumentException(
          "Settings.apply requires format \"names rings [positions] reflector\" (e.g. \"III-II-I AAA AAZ B\")"
        )

object Enigma:

  import Functor.tryFunctor

  object default:
    // Expose common givens and types when using this cipher's default package
    export cryptic.default.{given, *}
    export Enigma.{given, *}

  given encrypt(using settings: String): Encrypt[Try] =
    (plaintext: PlainText) =>
      val base = Settings(settings)
      val n = base.rotors.size
      val start = Glyph.random(n)
      val key = Glyph.random(n)
      val encryptedKey = run(key ++ key)(using base.pos(start))
      val encryptedMessage = run(plaintext.bytes)(using base.pos(key))
      val cipher =
        CipherText(start.iarray, encryptedKey.iarray, encryptedMessage.iarray)
      Success(cipher)

  given decrypt(using settings: String): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(startBytes, encryptedKeyBytes, encryptedMessageBytes) =>
        val base = Settings(settings)
        val n = base.rotors.size
        val start = glyph(startBytes)
        val encryptedKey = glyph(encryptedKeyBytes)
        val encryptedMessage = glyph(encryptedMessageBytes)
        val doubleKey = run(encryptedKey)(using base.pos(start))
        val key = doubleKey.take(n)
        val message = run(encryptedMessage)(using base.pos(key))
        Try(PlainText(message.string))

  def run(message: IArray[Byte])(using Settings): IArray[Glyph] = run(
    glyph(message)
  )

  def run(message: IArray[Glyph])(using settings: Settings): IArray[Glyph] =
    var rotors = settings.rotors
    message
      .map: g =>
        // Rotate all rotors applying ripple-carry rules
        rotors = rotors.rotate
        // Forward through rotors, reflect, then backward out
        val (forward, inTrace) = rotors.in(g)
        val reflected = settings.reflector.reflect(forward)
        val (output, outTrace) = rotors.out(reflected)
        scribe.trace(s"encoded ${inTrace.string}-${outTrace.string}")
        output

  def glyph(bytes: IArray[Byte]): IArray[Glyph] =
    new String(bytes.mutable).glyph
