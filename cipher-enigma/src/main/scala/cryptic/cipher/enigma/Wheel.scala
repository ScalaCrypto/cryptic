package cryptic
package cipher
package enigma

import scala.util.Try

case class Wheel(name: String, wiring: IArray[Glyph], notches: IArray[Glyph]):
  def carry(pos: Glyph): Boolean = notches.contains(pos)
  def in(g: Glyph): Glyph = wiring(g.int)
  def out(g: Glyph): Glyph = Glyph(wiring.indexOf(g))
  override def toString: String = name

object Wheel:
  // Predefined rotor singletons (kept private) to preserve reference and structural equality
  // while "inlining" access through the name-based apply.
  private lazy val predefined: Map[String, Wheel] =
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
      name -> Wheel(name, wiring, notch)
    .toMap

  /** Construct a Rotor from notch and wiring strings. Non‑alphabetic characters
    * are ignored (per `String.glyph`).
    */
  def apply(name: String, wiring: String, notch: String): Wheel =
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
    Wheel(name, w, n)

  /** Lookup a predefined rotor by its conventional name ("I" .. "VI"). */
  def apply(name: String): Wheel = predefined.getOrElse(
    name,
    throw IllegalArgumentException(s"Unknown rotor name: '$name'")
  )

  def unsafe(name: String): Try[Wheel] = Try(apply(name))
