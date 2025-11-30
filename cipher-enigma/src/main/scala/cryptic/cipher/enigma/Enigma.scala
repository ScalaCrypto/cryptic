package cryptic
package cipher
package enigma

import scala.util.{Success, Try}

case class Settings(rotors: Rotors, reflector: Reflector, plugboard: PlugBoard):
  /**
   * Adjusts the rotor positions using the given array of glyphs and returns an updated Settings object.
   *
   * @param pos An immutable array of Glyph objects representing the rotor positions to be set.
   * @return A new Settings instance with the updated rotor positions.
   */
  def pos(pos: IArray[Glyph]): Settings = copy(rotors = rotors.pos(pos))

object Settings:
  /**
   * Parses a settings string to construct a `Settings` object with the specified
   * rotors, reflector, and plugboard configuration.
   *
   * The settings string must follow the format:
   * "names rings [positions] reflector [plugboard]"
   * - `names`: Hyphen-separated rotor identifiers from left-most to right-most, e.g., "III-II-I".
   * - `rings`: A sequence of letters representing the ring settings, e.g., "AAA".
   * - `positions` (optional): A sequence of letters representing the initial rotor positions, e.g., "AAZ".
   * If omitted, defaults to "A" for each rotor.
   * - `reflector`: A single letter indicating the reflector type, e.g., "B".
   * - `plugboard` (optional): A string of paired letters representing plugboard connections, e.g., "ABCD".
   *
   * @param settings A string specifying the rotor names, ring settings, initial rotor positions,
   *                 reflector type, and optional plugboard configuration.
   * @return A `Settings` instance constructed based on the provided settings string.
   * @throws IllegalArgumentException If the settings string does not match the required format or contains
   *                                  invalid values.
   */
  def apply(settings: String): Settings =
    // Match four parts: names, rings, positions, reflector
    val WithPosPB =
      """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s+([A-Za-z]+)\s*$""".r
    val WithPos =
      """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s*$""".r
    val NoPosPB =
      """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s+([A-Za-z]+)\s*$""".r
    val NoPos = """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Ca-c])\s*$""".r

    settings match
      case WithPosPB(namesPart, ringsPart, posPart, reflPart, plugPairs) =>
        val rotors = Rotors(s"$namesPart $ringsPart $posPart")
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        val plugboard = PlugBoard(plugPairs)
        Settings(rotors, reflector, plugboard)
      case WithPos(namesPart, ringsPart, posPart, reflPart) =>
        val rotors = Rotors(s"$namesPart $ringsPart $posPart")
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        Settings(rotors, reflector, PlugBoard(""))
      case NoPosPB(namesPart, ringPart, reflPart, plugPairs) =>
        val rotors = Rotors(s"$namesPart $ringPart ${"A" * ringPart.length}")
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        val plugboard = PlugBoard(plugPairs)
        Settings(rotors, reflector, plugboard)
      case NoPos(namesPart, ringPart, reflPart) =>
        val rotors = Rotors(s"$namesPart $ringPart ${"A" * ringPart.length}")
        val reflector = Reflector.valueOf(reflPart.toUpperCase)
        Settings(rotors, reflector, PlugBoard(""))
      case _ =>
        throw IllegalArgumentException(
          """Settings requires format "names rings [positions] reflector [plugboard]" (e.g. "III-II-I AAA AAZ B ABCD")"""
        )

object Enigma:

  import Functor.tryFunctor

  object default:
    // Expose common givens and types when using this cipher's default package
    export cryptic.default.{given, *}
    export Enigma.{given, *}

  given encrypt(using base: Settings): Encrypt[Try] =
    (plaintext: PlainText) =>
      val n = base.rotors.size
      val start = Glyph.random(n)
      val key = Glyph.random(n)
      val encryptedKey = run(key ++ key)(using base.pos(start))
      val encryptedMessage = run(plaintext.bytes)(using base.pos(key))
      val cipher =
        CipherText(start.iarray, encryptedKey.iarray, encryptedMessage.iarray)
      Success(cipher)

  given decrypt(using base: Settings): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(startBytes, encryptedKeyBytes, encryptedMessageBytes) =>
        val n = base.rotors.size
        val start = glyph(startBytes)
        val encryptedKey = glyph(encryptedKeyBytes)
        val encryptedMessage = glyph(encryptedMessageBytes)
        val doubleKey = run(encryptedKey)(using base.pos(start))
        val key = doubleKey.take(n)
        val message = run(encryptedMessage)(using base.pos(key))
        Try(PlainText(message.string))

  def run(message: IArray[Byte])(using Settings): IArray[Glyph] = run(message.glyph)
  def run(message: IArray[Glyph])(using settings: Settings): IArray[Glyph] =
    var rotors = settings.rotors
    message
      .map: g =>
        // Rotate all rotors applying ripple-carry rules
        rotors = rotors.rotate
        // Plugboard in, forward through rotors, reflect, then backward out
        val swappedIn = settings.plugboard.swap(g)
        val (forward, inTrace) = rotors.in(swappedIn)
        val reflected = settings.reflector.reflect(forward)
        val (output, outTrace) = rotors.out(reflected)
        val swappedOut = settings.plugboard.swap(output)
        scribe.trace(s"encoded ${inTrace.string}-${outTrace.string}")
        swappedOut

