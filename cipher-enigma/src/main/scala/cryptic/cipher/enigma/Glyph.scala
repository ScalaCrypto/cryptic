package cryptic
package cipher
package enigma

import java.security.SecureRandom
import scala.util.{Failure, Success, Try}

val secureRandom = SecureRandom()
opaque type Glyph = Int

object Glyph:
  val mod = 26
  val base = 65
  val one: Glyph = Glyph(1)
  def apply(i: Int): Glyph = (i + mod) % mod
  def apply(c: Char): Glyph = apply(c.intValue - base)
  def random(n: Int): IArray[Glyph] =
    IArray.from((0 until n).map(_ => Glyph(secureRandom.nextInt(mod))))

  /** Safely construct a Glyph from a Char.
    *   - Accepts A–Z or a–z; lower-case is converted to upper-case first.
    *   - Returns Failure(IllegalArgumentException) for any non A–Z/a–z
    *     character.
    */
  def unsafe(c: Char): Try[Glyph] =
    val upper = if c >= 'a' && c <= 'z' then c.toUpper else c
    if upper >= 'A' && upper <= 'Z' then Success(apply(upper))
    else Failure(IllegalArgumentException(s"Invalid character for Glyph: '$c'"))

  /** Returns true if the provided character can be converted to a Glyph
    * according to the same rules as `unsafe` (i.e., letters A–Z/a–z only).
    */
  def isValid(c: Char): Boolean = Glyph.unsafe(c).isSuccess

extension (g: Glyph)
  def char: Char = (g + Glyph.base).toChar
  def string: String = char.toString
  def byte: Byte = (g + Glyph.base).byteValue

  /** Underlying 0-25 index value of this Glyph */
  def int: Int = g
  def +(term: Glyph): Glyph = Glyph(g + term)
  def -(term: Glyph): Glyph = Glyph(g - term)
  def ++ : Glyph = Glyph(g + 1)
  def -- : Glyph = Glyph(g - 1)

extension (c: Char)
  def glyph: Glyph = Glyph(c)
  def isGlyph: Boolean = Glyph.isValid(c)

extension (s: String)
  /** Converts the characters of a string into an immutable array of Glyphs.
    * Uses `Glyph.unsafe` for each character and ignores any failures, meaning
    * only the safe alphabetic characters (A–Z/a–z) are included. Lower-case
    * letters are treated as their upper-case equivalents by `unsafe`.
    */
  def glyph: IArray[Glyph] =
    val kept: Array[Glyph] =
      s.toCharArray.flatMap(c => Glyph.unsafe(c).toOption)
    IArray.from(kept)

extension (ga: IArray[Glyph])
  /** Convert an immutable array of Glyphs to a String
    */
  def string: String = ga.map(_.char).mkString

extension (iter: Iterable[Glyph])
  /** Constructs a string by concatenating the `char` representation of elements
    * in the sequence `seq`.
    *
    * @return
    *   A string composed of the characters obtained from the `char` property of
    *   each element in the sequence `seq`.
    */
  def string: String = iter.map(_.char).mkString
  def iarray: IArray[Byte] = IArray.from(iter.map(_.byte))

extension (iarray:IArray[Byte])
  def glyph:IArray[Glyph] = new String(iarray.mutable).glyph