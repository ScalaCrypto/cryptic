package cryptic
package cipher
package enigma

/** Simple Enigma plugboard implementation.
  *
  * @param wiring
  *   Zero to ten disjoint pairs of unique `Glyph`s. Each `Glyph` may appear at
  *   most once across all pairs, and the two `Glyph`s within a pair must
  *   differ.
  */
case class PlugBoard(wiring: Seq[(Glyph, Glyph)]):
  require(wiring.length <= 10, "PlugBoard may contain at most 10 pairs")
  require(
    wiring.forall((a, b) => a != b),
    "PlugBoard pairs must map two different letters (no self-pairs)"
  )
  private val allGlyphs: IArray[Glyph] =
    IArray.from(wiring.flatMap((a, b) => Array(a, b)))
  require(
    allGlyphs.distinct.length == allGlyphs.length,
    "PlugBoard letters must be unique across all pairs"
  )

  override def toString: String = wiring.map((f,t)=>s"${f.string}${t.string}").mkString

  /** Swap the provided `Glyph` using the configured wiring. If the `Glyph` is
    * not present in any pair, it is returned unchanged.
    */
  def swap(g: Glyph): Glyph =
    wiring
      .collectFirst:
        case (a, b) if g == a => b
        case (a, b) if g == b => a
      .getOrElse(g)

object PlugBoard:
  val empty = PlugBoard(Seq.empty)
  /** Construct a PlugBoard from a concatenated string of letter pairs with no
    * spaces, e.g. "ABCD" -> pairs (A,B), (C,D). Lowercase letters are accepted
    * and normalized to uppercase. The total number of pairs must be between 0
    * and 10 (i.e. length 0 to 20), and letters must be unique.
    */
  def apply(pairs: String): PlugBoard =
    val s = pairs.trim
    if s.nonEmpty && !s.forall(_.isLetter) then
      throw IllegalArgumentException(
        "PlugBoard.apply requires only letters A–Z or a–z"
      )
    val glyphs: IArray[Glyph] = s.glyph
    if glyphs.length != s.length then
      throw IllegalArgumentException(
        "PlugBoard.apply contains invalid characters"
      )
    if glyphs.length % 2 != 0 then
      throw IllegalArgumentException(
        "PlugBoard.apply requires an even number of letters (pairs)"
      )
    val pairCount = glyphs.length / 2
    if pairCount > 10 then
      throw IllegalArgumentException(
        "PlugBoard may contain at most 10 pairs (20 letters)"
      )

    val wiring = IArray.tabulate(pairCount): i =>
      (glyphs(i * 2), glyphs(i * 2 + 1))

    new PlugBoard(wiring)
