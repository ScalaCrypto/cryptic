package cryptic
package cipher
package enigma

/** Enigma reflectors A, B, C.
  *
  * Wiring is expressed in the domain type `Glyph` and the primary API operates
  * on `Glyph`, with convenience overload for `Char`.
  */
enum Reflector(wiring: IArray[Glyph]):
  /** Primary reflect method operating on Glyph domain values */
  def reflect(g: Glyph): Glyph = wiring(g.int)

  /** Convenience overload reflecting a Char */
  def reflect(c: Char): Char = reflect(c.glyph).char

  case A extends Reflector("EJMZALYXVBWFCRQUONTSPIKHGD".glyph)
  case B extends Reflector("YRUHQSLDPXNGOKMIEBFZCWVJAT".glyph)
  case C extends Reflector("FVPJIAOYEDRZXWGCTKUQSBNMHL".glyph)
