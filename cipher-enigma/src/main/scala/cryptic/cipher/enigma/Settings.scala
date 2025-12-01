package cryptic
package cipher
package enigma

case class Settings(
    rotors: Rotors,
    reflector: Reflector,
    plugboard: PlugBoard,
    preamble: Boolean
):
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
        val posOption = Option(posOrNull)
        val preamble = posOption.isEmpty
        val pos = posOption.getOrElse("A" * rings.length)
        val rotors = Rotors(s"$names $rings $pos")
        val reflector = Reflector.valueOf(refl.toUpperCase)
        val plugboard = PlugBoard(pb)
        Settings(rotors, reflector, plugboard, preamble)
      case _ =>
        throw IllegalArgumentException(
          """Settings requires format "names rings [positions] reflector [plugboard]" (e.g. "III-II-I AAA AAZ B ABCD")"""
        )
