package cryptic

import scala.util.{Failure, Success, Try}

trait Version:
  def bytes: IArray[Byte]
  def supports(version: IArray[Byte]): Boolean
  def failed[A](version: IArray[Byte]): Failure[A]

/** Holds an expected version header and validates actual headers against it. */
final case class FixedVersion(bytes: IArray[Byte])
    extends Version
    with Ordered[FixedVersion]:
  require(
    bytes.length == 4,
    s"Version must be exactly four bytes, got ${bytes.length}"
  )

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

object FixedVersion:
  def apply(x: Byte, xs: Byte*): FixedVersion = new FixedVersion(IArray(x, xs*))
