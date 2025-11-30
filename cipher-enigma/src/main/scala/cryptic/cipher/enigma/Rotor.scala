package cryptic
package cipher
package enigma

case class Rotor(wheel: Wheel, ring: Glyph, pos: Glyph):
  def rotate: Rotor = copy(pos = pos.++)
  val offset: Glyph = pos - ring
  def carry: Boolean = wheel.carry(pos)
  def previousCarry:Boolean = wheel.carry(pos.--)

  def in(g: Glyph): Glyph = wheel.in(g + offset) - offset
  def in(c: Char): Char = in(c.glyph).char

  def out(g: Glyph): Glyph = wheel.out(g + offset) - offset
  def out(c: Char): Char = out(c.glyph).char

  override def toString: String = s"Rotor($wheel ${ring.char} ${pos.char})"

object Rotor:
  /** Convenience constructor for tests and callers that prefer simple types.
    * Builds a `Rotor` from a rotor name, ring setting as a character (A–Z), and
    * a character position.
    */
  def apply(rotorName: String, ring: Char, pos: Char): Rotor =
    Rotor(Wheel(rotorName), ring.glyph, pos.glyph)

  /** Convenience constructor from a single settings string with the format:
    * "wheel ring pos"
    *
    * Examples:
    *   - "I A A"
    *   - "II a b" (lowercase letters are accepted for ring/pos)
    *   - Extra internal whitespace is tolerated: "III A Z"
    *
    * @throws IllegalArgumentException
    *   if the format is invalid, the wheel name is unknown, or ring/pos are not
    *   alphabetic characters.
    */
  def apply(settings: String): Rotor =
    // Regex that tolerates leading/trailing whitespace, requires exactly three tokens, and
    // enforces single alphabetic characters for ring and pos.
    val Settings = """^\s*([^\s]+)\s+([A-Za-z])\s+([A-Za-z])\s*$""".r

    settings match
      case Settings(name, ringStr, posStr) =>
        val triedRotor = for
          wheel <- Wheel.unsafe(name)
          ring <- Glyph.unsafe(ringStr.head)
          pos <- Glyph.unsafe(posStr.head)
        yield Rotor(wheel, ring, pos)

        triedRotor.get
      case _ =>
        throw IllegalArgumentException(
          "Rotor.apply requires format \"wheel ring pos\" with ring/pos as letters A-Z or a-z"
        )
