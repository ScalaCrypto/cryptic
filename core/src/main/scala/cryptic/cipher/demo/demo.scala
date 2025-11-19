package cryptic
package cipher
package demo

import scala.util.{Failure, Success, Try}

case class Key(offset: Int)

extension (bytes: IArray[Byte])
  /** Adds an offset to each byte in the input array, returning the resulting
    * array within the effect type `F`. If the offset is 0, it produces a
    * failure with an appropriate error message.
    *
    * @param offset
    *   the integer value to be added to each byte in the input array; cannot be
    *   0
    * @param functor
    *   an instance of the `Functor` type class providing operations for the
    *   effect type `F`
    * @return
    *   an effectful computation containing the resulting `IArray[Byte]` with
    *   the applied offset, or a failure if the offset is 0
    */
  def addOffset[F[_]: Functor](
      offset: Int
  )(using functor: Functor[F]): F[IArray[Byte]] =
    if offset == 0 then
      functor.failed(new IllegalArgumentException("Key offset cannot be 0"))
    else functor.pure(bytes.mutable.map(b ⇒ (b + offset).toByte).immutable)
