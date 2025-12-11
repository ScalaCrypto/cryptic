package cryptic
package cipher
package enigma

import scala.util.Try

/**
 * Represents a Wheel (or Rotor) used in an Enigma-like device.
 *
 * A Wheel defines a specific wiring configuration and one or more notches 
 * that determine when it triggers a carry operation to the next rotor.
 *
 * @constructor Creates a Wheel with a specific name, wiring configuration, and
 *              associated notches.
 * @param name    The name of the wheel, typically indicating its type or version.
 * @param wiring  An indexed sequence of Glyphs representing the wiring configuration 
 *                used for letter substitution.
 * @param notches An indexed sequence of Glyphs indicating the positions at 
 *                which this wheel causes a carry operation.
 */
case class Wheel(name: String, wiring: IArray[Glyph], notches: IArray[Glyph]):
  def carry(pos: Glyph): Boolean = notches.contains(pos)
  def in(g: Glyph): Glyph = wiring(g.int)
  def out(g: Glyph): Glyph = Glyph(wiring.indexOf(g))
  override def toString: String = name

/**
 * Companion object for the `Wheel` class, providing factory methods and predefined configurations.
 *
 * The `Wheel` object allows for the creation of `Wheel` instances, either from scratch with
 * specified wiring and notch configurations, or by referencing predefined configurations
 * by their conventional names (e.g., "I", "II", etc.).
 */
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
