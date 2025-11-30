package cryptic
package cipher
package enigma

case class Rotors(rotors: IArray[Rotor]):
  require(
    rotors.nonEmpty,
    "Rotors must contain at least one rotor state (right-most at index 0)"
  )
  require(
    rotors.map(_.wheel.name).distinct.length == rotors.length,
    "Rotors state must not contain duplicate rotors"
  )

  /** Transforms a given `Glyph` through the sequence of rotors, recording the
    * state of the `Glyph` at each step.
    *
    * @param g
    *   The initial `Glyph` to be transformed.
    * @return
    *   A tuple containing the final `Glyph` after transformation and a sequence
    *   of `Glyph` states after each rotor's transformation, including the
    *   initial state.
    */
  def in(g: Glyph): (Glyph, Seq[Glyph]) =
    rotors.foldLeft((g, Seq(g))): (acc, rotor) =>
      val h = rotor.in(acc._1)
      (h, acc._2 :+ h)

  /** Convenience overload for chars. */
  def in(c: Char): Char = in(c.glyph)._1.char

  /** Transforms the given `Glyph` object by applying a sequence of states in
    * reverse order.
    *
    * @param g
    *   the initial `Glyph` to be transformed
    * @return
    *   the transformed `Glyph` after applying the states
    */
  def out(g: Glyph): (Glyph, Seq[Glyph]) =
    rotors.foldRight((g, Seq(g))): (rotor, acc) =>
      val h = rotor.out(acc._1)
      (h, acc._2 :+ h)

  /** Convenience overload for chars. */
  def out(c: Char): Char = out(c.glyph)._1.char

  def pos: IArray[Glyph] = rotors.map(_.pos)

  override def toString: String = s"""Rotors(${rotors
      .map(_.wheel.name)
      .reverse
      .mkString("-")} ${pos.string.reverse})"""

  /** Rotate the rotors applying Enigma carry rules for an arbitrary number of
    * rotors. States are ordered from right to left (index 0 = right-most,
    * highest speed):
    *   - The right-most rotor (index 0) always rotates.
    *   - A carry from rotor i after its rotation causes rotor i+1 to rotate,
    *     and so on.
    */
  def rotate: Rotors =
    val (rotated, _) = rotors.zipWithIndex.foldLeft((Seq.empty[Rotor], true)):
      case ((seq, _), (rotor, 0)) =>
        val next = rotor.rotate
        (seq :+ next, next.carry)
      case ((seq, carry), (rotor, 1)) =>
        // We need to handle double step anomaly
        val rotated = rotor.rotate
        val (c, n) = (carry, rotor.carry, rotated.carry) match
          case (true, false, true)   => (true, rotated)
          case (true, _, true)   => (false, rotated)
          case (true, _, false)  => (false, rotated)
          case (false, _, true)  => (true, rotated)
          case (false, _, false) => (false, rotor)
        (seq :+ n, c)
      case ((seq, true), (rotor, _)) =>
        val rotated = rotor.rotate
        (seq :+ rotated, rotated.carry)
      case ((seq, false), (rotor, _)) =>
        (seq :+ rotor, rotor.carry)
    val next = Rotors(IArray.from(rotated))
    scribe.info(s"Rotated ${pos.string.reverse} -> ${next.pos.string.reverse}")
    next

object Rotors:
  /** Convenience constructor allowing varargs of RotorState with a minimum of
    * one. Right-most rotor should be provided first (index 0), matching the
    * IArray ordering used by the primary case class.
    */
  def apply(head: Rotor, tail: Rotor*): Rotors =
    val arr: IArray[Rotor] = IArray.from(head +: tail.toSeq)
    Rotors(arr)

  /** Convenience constructor from a single settings string with the format:
    * "names rings positions"
    *
    * Names are hyphen-separated rotor identifiers from left-most to right-most
    * (e.g. "VI-II-I"). Ring and position are sequences of letters whose lengths
    * must match the number of names, and are given left-most to right-most
    * (e.g. "ABC DEF"). Lowercase letters are accepted for ring/pos and
    * normalized to uppercase.
    *
    * Example:
    *   - "VI-II-I ABC DEF" => Rotors(Rotor("I C F"), Rotor("II B E"), Rotor("VI
    *     A D"))
    *
    * The internal ordering of `Rotors` is right-most first (index 0), so the
    * resulting sequence is reversed relative to the names/rings/positions
    * provided (which are left-most to right-most).
    *
    * @throws IllegalArgumentException
    *   if the format is invalid, lengths are mismatched, names are empty, or
    *   ring/pos contain non-alphabetic characters, or if any wheel name is
    *   unknown.
    */
  def apply(settings: String): Rotors =
    // Tolerate leading/trailing whitespace; require exactly three whitespace-separated parts
    val Settings = """^\s*([^\s]+)\s+([A-Za-z]+)\s+([A-Za-z]+)\s*$""".r

    settings match
      case Settings(namesPart, ringsPart, posPart) =>
        val names = namesPart.split("-").map(_.trim).toVector
        if names.isEmpty || !names.forall(_.nonEmpty) then
          throw IllegalArgumentException(
            "Rotors.apply requires non-empty hyphen-separated rotor names (e.g. 'VI-II-I')"
          )

        val n = names.length
        if !(ringsPart.length == n && posPart.length == n) then
          throw IllegalArgumentException(
            s"Rotors.apply rings and positions must have length $n to match names count"
          )

        // Validate letters (regex already restricts A-Za-z but double-check defensively)
        if !(ringsPart.forall(_.isLetter) && posPart.forall(_.isLetter)) then
          throw IllegalArgumentException(
            "Rotors.apply requires ring/pos to be letters A-Z or a-z only"
          )

        // Build rotors in right-most-first order (reverse of input order)
        val rotors: Seq[Rotor] = (0 until n).reverse.map: i =>
          val name = names(i)
          val ringCh = ringsPart.charAt(i).toUpper
          val posCh = posPart.charAt(i).toUpper
          Rotor(name, ringCh, posCh)

        Rotors(IArray.from(rotors))
      case _ =>
        throw IllegalArgumentException(
          "Rotors.apply requires format \"names rings positions\" (e.g. \"VI-II-I ABC DEF\") with ring/pos as letters A-Z or a-z"
        )
