package cryptic
package cipher
package enigma

import scala.util.{Failure, Success, Try}

case class Settings(rotors: Rotors, reflector: Reflector, plugboard: PlugBoard):
  /** Adjusts the rotor positions using the given array of glyphs and returns an
    * updated Settings object.
    *
    * @param pos
    *   An immutable array of Glyph objects representing the rotor positions to
    *   be set.
    * @return
    *   A new Settings instance with the updated rotor positions.
    */
  def pos(pos: IArray[Glyph]): Settings = copy(rotors = rotors.pos(pos))

object Settings:
  /** Parses a settings string to construct a `Settings` object with the
    * specified rotors, reflector, and plugboard configuration.
    *
    * The settings string must follow the format: "names rings [positions]
    * reflector [plugboard]"
    *   - `names`: Hyphen-separated rotor identifiers from left-most to
    *     right-most, e.g., "III-II-I".
    *   - `rings`: A sequence of letters representing the ring settings, e.g.,
    *     "AAA".
    *   - `positions` (optional): A sequence of letters representing the initial
    *     rotor positions, e.g., "AAZ". If omitted, defaults to "A" for each
    *     rotor.
    *   - `reflector`: A single letter indicating the reflector type, e.g., "B".
    *   - `plugboard` (optional): A string of paired letters representing
    *     plugboard connections, e.g., "ABCD".
    *
    * @param settings
    *   A string specifying the rotor names, ring settings, initial rotor
    *   positions, reflector type, and optional plugboard configuration.
    * @return
    *   A `Settings` instance constructed based on the provided settings string.
    * @throws IllegalArgumentException
    *   If the settings string does not match the required format or contains
    *   invalid values.
    */
  def apply(settings: String): Settings =
    // Match parts: names, rings, optional positions, reflector, optional plugboard
    val SettingsFormat =
      """^\s*(\S+)\s+([A-Za-z]+)\s+(?:([A-Za-z]+)\s+)?([A-Ca-c])\s*([A-Za-z]*)\s*$""".r

    settings match
      case SettingsFormat(names, rings, posOrNull, refl, pb) =>
        val pos = Option(posOrNull).getOrElse("A" * rings.length)
        val rotors = Rotors(s"$names $rings $pos")
        val reflector = Reflector.valueOf(refl.toUpperCase)
        val plugboard = PlugBoard(pb)
        Settings(rotors, reflector, plugboard)
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

  given encrypt(using settings: Settings): Encrypt[Try] =
    (plaintext: PlainText) =>
      val n = settings.rotors.size
      val start = Glyph.random(n)
      val key = Glyph.random(n)
      val encryptedKey = run(key ++ key)(using settings.pos(start))
      val encryptedMessage = run(plaintext.bytes)(using settings.pos(key))
      val cipher =
        CipherText(start.iarray, encryptedKey.iarray, encryptedMessage.iarray)
      Success(cipher)

  given decrypt(using settings: Settings): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(startBytes, encryptedKeyBytes, encryptedMessageBytes) =>
        val n = settings.rotors.size
        val start = glyph(startBytes)
        val encryptedKey = glyph(encryptedKeyBytes)
        val encryptedMessage = glyph(encryptedMessageBytes)
        val doubleKey = run(encryptedKey)(using settings.pos(start))
        val (key1, key2) = doubleKey.splitAt(n)
        if key1 == key2
        then
          Failure(
            IllegalArgumentException(
              s"Inconsistent key in preamble ${start.string} ${doubleKey.string}"
            )
          )
        else
          val message = run(encryptedMessage)(using settings.pos(key1))
          Success(PlainText(message.string))

  def run(message: IArray[Byte])(using Settings): IArray[Glyph] = run(
    message.glyph
  )
  def run(message: IArray[Glyph])(using settings: Settings): IArray[Glyph] =
    var rotors = settings.rotors
    message
      .map: g =>
        rotors = rotors.rotate
        val swappedIn = settings.plugboard.swap(g)
        val (forward, inTrace) = rotors.in(swappedIn)
        val reflected = settings.reflector.reflect(forward)
        val (output, outTrace) = rotors.out(reflected)
        val swappedOut = settings.plugboard.swap(output)
        scribe.trace(
          s"encoded ${g.char} ${inTrace.string}-${outTrace.string} ${swappedOut.char}"
        )
        swappedOut
