package cryptic
package cipher
package enigma

import Glyph.*

case class Rotor(name: String, wiring: IArray[Glyph], notches: IArray[Glyph]):
  def carry(pos: Glyph): Boolean = notches.contains(pos)
  def in(g: Glyph): Glyph = wiring(g.int)
  def out(g: Glyph): Glyph = Glyph(wiring.indexOf(g))
  override def toString: String = name

object Rotor:
  // Predefined rotor singletons (kept private) to preserve reference and structural equality
  // while "inlining" access through the name-based apply.
  private lazy val predefined: Map[String, Rotor] =
    Array[
      (String, String, String)
    ](
      ("I", "EKMFLGDQVZNTOWYHXUSPAIBRCJ", "R"),
      ("II", "AJDKSIRUXBLHWTMCQGZNPYFVOE", "F"),
      ("III", "BDFHJLCPRTXVZNYEIWGAKMUSQO", "W"),
      ("IV", "ESOVPZJAYQUIRHXLNFTGKDCMWB", "K"),
      ("V", "VZBRGITYUPSDNHLXAWMJQOFECK", "A"),
      ("VI", "JPGVOUMFYQBENHZRDKASXLICTW", "AN")
    ).map: (name, wiring, notch) =>
      name -> Rotor(name, wiring, notch)
    .toMap

  /** Construct a Rotor from notch and wiring strings. Non‑alphabetic characters
    * are ignored (per `String.glyph`).
    */
  def apply(name: String, wiring: String, notch: String): Rotor =
    val n = notch.glyph
    val w = wiring.glyph
    require(
      n.nonEmpty,
      "Rotor notch must be non-empty (at least one valid A–Z char)"
    )
    require(
      n.distinct.length == n.length,
      "Rotor notch characters must be non-repeating"
    )
    require(
      w.length == Glyph.mod,
      s"Rotor wiring must have length ${Glyph.mod}"
    )
    Rotor(name, w, n)

  /** Lookup a predefined rotor by its conventional name ("I" .. "VI"). */
  def apply(name: String): Rotor =
    predefined.getOrElse(
      name,
      throw IllegalArgumentException(s"Unknown rotor name: '$name'")
    )

case class RotorState(rotor: Rotor, ring: Glyph, pos: Glyph):
  def rotate: RotorState = copy(pos = pos.++)
  def carry: Boolean = rotor.carry(pos + ring)

  def in(g: Glyph): Glyph = rotor.in(g + pos) - pos
  def in(c: Char): Char = in(c.glyph).char

  def out(g: Glyph): Glyph = rotor.out(g + pos) - pos
  def out(c: Char): Char = out(c.glyph).char

  override def toString: String =
    s"RotorState($rotor, ${"%02d".format(ring.int)}, ${pos.char})"

object RotorState:
  /** Convenience constructor for tests and callers that prefer simple types.
    * Builds a `RotorState` from a rotor name, numeric ring setting, and a
    * character position.
    */
  def apply(rotorName: String, ring: Int, pos: Char): RotorState =
    RotorState(Rotor(rotorName), Glyph(ring), pos.glyph)

case class Rotors(states: IArray[RotorState]):
  require(
    states.nonEmpty,
    "Rotors must contain at least one rotor state (right-most at index 0)"
  )
  require(
    states.map(_.rotor.name).distinct.length == states.length,
    "Rotors state must not contain duplicate rotors"
  )

  /** Pass a glyph through the rotor stack from right-most to left-most,
    * chaining each `RotorState.in` just like `Enigma.encrypt` does.
    */
  def in(g: Glyph): Glyph = states.foldLeft(g)((acc, s) => s.in(acc))

  /** Convenience overload for chars. */
  def in(c: Char): Char = in(c.glyph).char

  /** Pass a glyph back through the rotor stack from left-most to right-most,
    * chaining each `RotorState.out` in reverse order of `in`, matching
    * `Enigma.encrypt`'s return path after the reflector.
    */
  def out(g: Glyph): Glyph = states.foldRight(g)((s, acc) => s.out(acc))

  /** Convenience overload for chars. */
  def out(c: Char): Char = out(c.glyph).char

  /** Rotate the rotors applying Enigma carry rules for an arbitrary number of
    * rotors. States are ordered from right to left (index 0 = right-most,
    * highest speed):
    *   - The right-most rotor (index 0) always rotates.
    *   - A carry from rotor i after its rotation causes rotor i+1 to rotate,
    *     and so on.
    */
  def rotate: Rotors =
    val (rotated, _) = states.foldLeft((Seq[RotorState](), true)):
      case ((seq, true), state) =>
        val next = state.rotate
        (seq :+ next, next.carry)
      case ((seq, false), state) =>
        (seq :+ state, state.carry)
    Rotors(IArray.from(rotated))

object Rotors:
  /** Convenience constructor allowing varargs of RotorState with a minimum of one.
    * Right-most rotor should be provided first (index 0), matching the IArray
    * ordering used by the primary case class.
    */
  def apply(head: RotorState, tail: RotorState*): Rotors =
    val arr: IArray[RotorState] = IArray.from(head +: tail.toSeq)
    Rotors(arr)
