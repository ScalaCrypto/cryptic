package cryptic

import scala.util.{Failure, Success, Try}

trait Versioning:
  def withVersion[A](version: IArray[Byte])(f: => A): Try[A]
  def bytes: IArray[Byte]

extension (bytes: IArray[Byte])
  def withVersion[A](f: => A)(using v: Versioning): Try[A] =
    v.withVersion(bytes)(f)

/** Holds an expected version header and validates actual headers against it. */
final case class FixedVersion(bytes: IArray[Byte])
    extends Versioning
    with Ordered[FixedVersion]:
  require(
    bytes.length == 4,
    s"Version must be exactly four bytes, got ${bytes.length}"
  )

  /** Validates the provided version header against the expected version header.
    * If the version headers match, executes the specified action and returns
    * its result. If the version headers differ, returns a failure with an
    * `IllegalArgumentException`.
    *
    * @param actual
    *   the version header provided for validation
    * @param action
    *   the operation to be executed if the version headers match
    * @return
    *   a `Try` containing the result of the action if the version headers
    *   match, or a failure with an `IllegalArgumentException` if they do not
    */
  def withVersion[A](actual: IArray[Byte])(action: => A): Try[A] =
    if actual.mutable.sameElements(bytes.mutable) then Try(action)
    else
      Failure[A](
        new IllegalArgumentException(
          s"""version mismatch: only supports ${format(bytes)} got ${format(
              actual
            )}"""
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

object FixedVersion:
  def apply(x: Byte, xs: Byte*): FixedVersion =
    require(
      xs.length == 3,
      s"Version must be exactly 4 bytes, got ${1 + xs.length}"
    )
    new FixedVersion(IArray(x, xs*))
