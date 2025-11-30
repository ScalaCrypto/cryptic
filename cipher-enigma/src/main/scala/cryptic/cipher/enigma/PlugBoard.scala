package cryptic
package cipher
package enigma

/** Simple Enigma plugboard implementation.
  *
  * @param wiring
  *   Zero to ten disjoint pairs of unique `Glyph`s. Each `Glyph` may appear at
  *   most once across all pairs, and the two `Glyph`s within a pair must differ.
  */
case class PlugBoard(wiring: IArray[(Glyph, Glyph)])
  :
  // Validate constraints
  require(wiring.length <= 10, "PlugBoard may contain at most 10 pairs")
  require(
    wiring.forall((a, b) => a != b),
    "PlugBoard pairs must map two different letters (no self-pairs)"
  )
  private val allGlyphs: IArray[Glyph] = IArray.from(wiring.flatMap((a, b) => Array(a, b)))
  require(
    allGlyphs.distinct.length == allGlyphs.length,
    "PlugBoard letters must be unique across all pairs"
  )

  /** Swap the provided `Glyph` using the configured wiring. If the `Glyph`
    * is not present in any pair, it is returned unchanged.
    */
  def swap(g: Glyph): Glyph =
    // simple linear search across pairs; wiring limited to 10 so this is fine
    var i = 0
    while i < wiring.length do
      val (a, b) = wiring(i)
      if g == a then return b
      if g == b then return a
      i += 1
    g

object PlugBoard:
  /** Construct a PlugBoard from a concatenated string of letter pairs with no
    * spaces, e.g. "ABCD" -> pairs (A,B), (C,D). Lowercase letters are
    * accepted and normalized to uppercase. The total number of pairs must be
    * between 0 and 10 (i.e. length 0 to 20), and letters must be unique.
    */
  def apply(pairs: String): PlugBoard =
    val s = pairs.trim
    // Validate characters: only letters allowed
    if s.nonEmpty && !s.forall(_.isLetter) then
      throw IllegalArgumentException("PlugBoard.apply requires only letters A–Z or a–z")
    // Normalize to Glyphs (which upper-cases and validates)
    val glyphs: IArray[Glyph] = s.glyph
    if glyphs.length != s.length then
      // This would only happen if non-letters were present; be explicit
      throw IllegalArgumentException("PlugBoard.apply contains invalid characters")
    if glyphs.length % 2 != 0 then
      throw IllegalArgumentException("PlugBoard.apply requires an even number of letters (pairs)")
    val pairCount = glyphs.length / 2
    if pairCount > 10 then
      throw IllegalArgumentException("PlugBoard may contain at most 10 pairs (20 letters)")
    val arr = new Array[(Glyph, Glyph)](pairCount)
    var i = 0
    while i < pairCount do
      val a = glyphs(i * 2)
      val b = glyphs(i * 2 + 1)
      arr(i) = (a, b)
      i += 1
    PlugBoard(IArray.from(arr))
