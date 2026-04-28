package cryptic

import scala.util.Failure

/** Represents a version identifier for cryptographic formats.
  */
trait Version:
  /** The byte representation of this version. */
  def bytes: IArray[Byte]

  /** Checks if this version supports the given version bytes. */
  def supports(version: IArray[Byte]): Boolean

  /** Checks if this version supports another version. */
  def supports(version: Version): Boolean

  /** Creates a failure indicating an unsupported version. */
  def failed[A](version: IArray[Byte]): Failure[A]

/** Holds an expected version header and validates actual headers against it. */
final case class FixedVersion(b0: Byte, b1: Byte, b2: Byte, b3: Byte)
    extends Version
    with Ordered[FixedVersion]:

  val bytes: IArray[Byte] = IArray(b0, b1, b2, b3)

  def failed[A](version: IArray[Byte]): Failure[A] =
    Failure(
      new IllegalArgumentException(
        s"Unsupported version ${version.mkString(".")}, expected $this"
      )
    )

  def format(arr: IArray[Byte]): String = arr.mkString(".")
  override def toString: String = format(bytes)

  override def compare(that: FixedVersion): Int =
    val thisInt = (bytes(0) << 24) | ((bytes(1) & 0xff) << 16) |
      ((bytes(2) & 0xff) << 8) | (bytes(3) & 0xff)
    val thatInt =
      (that.bytes(0) << 24) | ((that.bytes(1) & 0xff) << 16) |
        ((that.bytes(2) & 0xff) << 8) | (that.bytes(3) & 0xff)
    thisInt - thatInt

  override def supports(version: IArray[Byte]): Boolean =
    bytes.sameElements(version)

  override def supports(version: Version): Boolean = supports(version.bytes)

object FixedVersion:
  def apply(bytes: IArray[Byte]): FixedVersion =
    val length = bytes.length
    require(
      length == 4,
      s"FixedVersion must be exactly four bytes, got $length"
    )
    new FixedVersion(bytes(0), bytes(1), bytes(2), bytes(3))
  def apply(b0: Int, b1: Int, b2: Int, b3: Int): FixedVersion =
    new FixedVersion(b0.toByte, b1.toByte, b2.toByte, b3.toByte)
