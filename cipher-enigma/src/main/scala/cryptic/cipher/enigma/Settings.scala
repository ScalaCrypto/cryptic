package cryptic
package cipher
package enigma

import scala.util.{Failure, Success, Try}

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
  /**
   * Constructs a new `Settings` instance by parsing the provided configuration string.
   *
   * @param settings
   * A string containing the configuration settings in the format
   * `"names rings [positions] reflector [plugboard]"`.
   * @return
   * The `Settings` instance constructed from the parsed configuration.
   * Throws an exception if the string format is invalid.
   */
  def apply(settings: String): Settings = parse(settings).get
  /**
   * Parses a configuration string and attempts to construct a `Settings` instance.
   *
   * @param settings
   * A string containing the configuration settings in the format
   * `"names rings [positions] reflector [plugboard]"`. For example,
   * `"III-II-I AAA AAZ B ABCD"`.
   * @return
   * A `Try[Settings]` containing the successfully parsed `Settings` instance,
   * or a `Failure` with an `IllegalArgumentException` if the string format is invalid.
   */
  def parse(settings: String): Try[Settings] =
    // Match parts: names, rings, optional positions, reflector, optional plugboard
    val SettingsFormat =
      """^\s*(\S+)\s+([A-Za-z]+)\s+(?:([A-Za-z]+)\s+)?([A-Ca-c])(?:\s+([A-Za-z]*))?\s*$""".r

    settings match
      case SettingsFormat(names, rings, posOrNull, refl, pbOrNull) =>
        val posOption = Option(posOrNull)
        val preamble = posOption.isEmpty
        val pos = posOption.getOrElse("A" * rings.length)
        val rotors = Rotors(s"$names $rings $pos")
        val reflector = Reflector.valueOf(refl.toUpperCase)
        val plugboard = PlugBoard(Option(pbOrNull).getOrElse(""))
        Success(Settings(rotors, reflector, plugboard, preamble))
      case _ =>
        Failure(
          IllegalArgumentException(
            s"""Invalid settings: $settings. Required format "names rings [positions] reflector [plugboard]" (e.g. "III-II-I AAA AAZ B ABCD")"""
          )
        )
