package cryptic

import cryptic.serialization.Serializer
import cryptic.syntax._

import scala.language.implicitConversions

sealed abstract class Cryptic[V : Serializer] {
  import Cryptic._
  /** Returns this $cryptic's decrypted value to the right if it is nonempty,
    * otherwise returns an error message to the left.
    *
    *  @param decrypt the `Decrypt` instance to use for decryption.
    */
  def decrypted(implicit decrypt: Decrypt): Either[String, V]
  /** Returns this $cryptic's decrypted value if it is nonempty, otherwise
    * return the result of evaluating `default`.
    *
    *  @param default  the default expression.
    *  @param decrypt the `Decrypt` instance to use for decryption.
    */
  @inline final def decryptedOrElse[W >: V](default: => W)(implicit decrypt: Decrypt): W =
    decrypted.getOrElse(default)

  /** Returns a $cryptic which will apply $f to this $cryptic's
    * decrypted value (if this $cryptic is nonempty) once run.
    *
    *  @note This is similar to `flatMap` except here,
    *  $f does not need to wrap its result in a $crytic.
    *
    *  @param  f   the function to apply
    *  @see flatMap
    *  @see foreach
    */
  @inline final def map[W : Serializer](f: V => W): Operation[W] = Mapped(this, f)

  /** Returns a $cryptic which will apply $f to this $cryptic's
    * decrypted value (if this $cryptic is nonempty) once run.
    *
    * @note Slightly different from `map` in that $f is expected to
    * return a $cryptic (which could be $empty).
    *
    *  @param  f   the function to apply
    *  @see map
    *  @see foreach
    */
  @inline final def flatMap[W : Serializer](f: V => Cryptic[W]): Operation[W] = FlatMapped(this, f)

  /*
  /** Returns a $cryptic which will unwrap one level of cryptic nesting once run.
    *
    * @param  ev    evidence that this $cryptic's type parameter `V` is in fact a `Cryptic[W]`.
    */
  @inline final def flatten[W : Serializer](implicit ev: V <:< Cryptic[W]): Operation[W] = Flattened(this)
  */

  /** Returns a $cryptic which will apply the predicate $p to
    * this $crytic's decrypted value once run and if it is `true`
    * return this $cryptic, otherwise return $empty.
    *
    *  @param  p   the predicate used for testing.
    */
  @inline final def filter(p: V => Boolean): Operation[V] = Filtered(this, p)

  /** Returns a $cryptic which will apply the predicate $p to
    * this $crytic's decrypted value once run and if it is `false`
    * return this $cryptic, otherwise return $empty.
    *
    *  @param  p   the predicate used for testing.
    */
  @inline final def filterNot(p: V => Boolean): Filtered[V] = Filtered(this, v => !p(v))

  /** Returns a $cryptic which will contain the result of
    * applying `pf` to this $cryptic's decrypted value '''if''' this $cryptic is
    * nonempty '''and''' `pf` is defined for its value once run.
    *
    *  @param  pf   the partial function.
    *  @return the result of applying `pf` to this $option's
    *  value (if possible), or $none.
    */
  @inline final def collect[W : Serializer](pf: PartialFunction[V, W]): Operation[W] = Collected(this, pf)

  /** Returns a $cryptic which will be equal to this $cryptic if it is nonempty once run,
    *  otherwise will return the result of evaluating `alternative` once run.
    *  @param alternative the alternative expression.
    */
  @inline final def orElse[W >: V : Serializer](alternative: => Cryptic[W]): Operation[W] = new OrElsed(this, alternative)
}
object Cryptic {
  import Encrypted._
  sealed abstract class Operation[V : Serializer] extends Cryptic[V] {
    def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]]
  }
  sealed abstract class BinaryOperation[V : Serializer, W : Serializer] extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Encrypted(encrypt(implicitly[Serializer[W]].serialize(w))))
  }
  final case class Mapped[V : Serializer, W : Serializer](src: Cryptic[V], f: V => W)
      extends BinaryOperation[V, W] {
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.map(f)
  }
  final case class FlatMapped[V : Serializer, W : Serializer](src: Cryptic[V], f: V => Cryptic[W])
      extends BinaryOperation[V, W] {
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.flatMap[String, W](v => f(v).decrypted)
  }
  /*
  final case class Flattened[V : Serializer, W : Serializer](src: Cryptic[V])(implicit ev: V <:< Cryptic[W])
      extends Operation[V] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]] =
      src.decrypted.map[Encrypted[V]](ev.apply)
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.flatMap(v => ev(v).decrypted)
  }*/
  final case class Filtered[V : Serializer](src: Cryptic[V], pred: V => Boolean)
      extends Operation[V] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]] =
      src.decrypted.map[Encrypted[V]] { v ⇒
        if (pred(v)) v.encrypted
        else empty[V]
      }
    override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
      src.decrypted.flatMap(v => if (pred(v)) Right(v) else Left("decrypted called on filtered empty"))
  }
  final case class Collected[V : Serializer, W : Serializer](src: Cryptic[V], pf: PartialFunction[V, W])
      extends BinaryOperation[V, W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      src.decrypted match {
        case Right(v) if pf.isDefinedAt(v) => Right(pf(v).encrypted)
        case Right(_) => Right(empty[W])
        case Left(s) => Left(s)
      }
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted match {
        case Right(v) if pf.isDefinedAt(v) => Right(pf(v))
        case e => e.map(pf)
      }
  }
  final class OrElsed[V : Serializer, W >: V : Serializer](src: Cryptic[V], alternative: => Cryptic[W])
      extends BinaryOperation[V, W] {
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted match {
        case r @ Right(_) => r
        case Left(_) => alternative.decrypted
      }
  }
}
case class Encrypted[V : Serializer](cipherText: CipherText) extends Cryptic[V] {
  override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
    decrypt(cipherText).flatMap(implicitly[Serializer[V]].deserialize)
  /** Returns a byte array containing the encrypted data.
    * @return byte array with encrypted data
    */
  def bytes: Array[Byte] = cipherText.bytes

  /** Returns true if this $encrypted is $empty, false otherwise.
    */
  def isEmpty: Boolean = false

  /** Returns false if this $encrypted is $empty, true otherwise.
    *  @note   Implemented here to avoid the implicit conversion to Iterable.
    */
  @inline final def nonEmpty: Boolean = isDefined

  /** Returns true if this $encrypted is an not an instance of $empty, false otherwise.
    */
  @inline final def isDefined: Boolean = !isEmpty

  /** Tests whether this $encrypted contains a given value.
    *
    *  @param value     the value to compare to.
    *  @param decrypt   the `Decrypt` instance to use for decryption.
    *  @return `true` if this $cryptic has a value that is equal (as
    *  determined by `==`) to `value`, `false` otherwise.
    */
  @inline final def contains(value: V)(implicit decrypt: Decrypt): Boolean =
    !isEmpty && decrypted == value

  /** Returns true if this $encrypted is nonempty '''and''' the predicate
    * $p returns true when applied to its value, otherwise returns false.
    *
    *  @param  p        the predicate to test
    *  @param decrypt   the `Decrypt` instance to use for decryption.
    */
  @inline final def exists(p: V => Boolean)(implicit decrypt: Decrypt): Boolean =
    !isEmpty && decrypted.exists(p)

  /** Returns true if this $encrypted is empty '''or''' the predicate
    * $p returns true when applied to its value.
    *
    *  @param  p        the predicate to test
    *  @param decrypt   the `Decrypt` instance to use for decryption.
    */
  @inline final def forall(p: V => Boolean)(implicit decrypt: Decrypt): Boolean =
    isEmpty || decrypted.forall(p)

  /** Apply the given procedure $f to this $encrypted's value if it is nonempty,
    * otherwise do nothing.
    *
    *  @param  f        the procedure to apply.
    *  @param decrypt   the `Decrypt` instance to use for decryption.
    *  @see map
    *  @see flatMap
    */
  @inline final def foreach[U](f: V => U)(implicit decrypt: Decrypt): Unit =
    if (!isEmpty) decrypted.foreach(f)

  /** Returns a $cryptic which will apply $f to this $cryptic's
    * decrypted value (if the $cryptic is nonempty) once run.
    * If the $cryptic is $empty once run, expression `ifEmpty`
    * will be evaluated instead.
    *
    * @note This is equivalent to `$cryptic map f decryptedOrElse ifEmpty`.
    *
    * @param  ifEmpty the expression to evaluate if empty.
    * @param  f       the function to apply if nonempty.
    */
  @inline final def fold[W : Serializer](ifEmpty: => W)(f: V => W)(implicit decrypt: Decrypt): Either[String, W] =
    if (isEmpty) Right(ifEmpty) else decrypted.map(f)

  /** Returns a singleton iterator of this $cryptic's value if it is nonempty,
    * or the empty iterator otherwise.
    */
  @inline final def iterator(implicit decrypt: Decrypt): Either[String, Iterator[V]] =
    if (isEmpty) Right(collection.Iterator.empty) else decrypted.map(collection.Iterator.single)

  /** Returns a singleton list containing this $cryptics's value if it is nonempty,
    * or the empty list otherwise.
    */
  @inline final def toList(implicit decrypt: Decrypt): Either[String, List[V]] =
    if (isEmpty) Right(List()) else decrypted.map(_ :: Nil)
}
object Encrypted {
  /** Returns a new $encrypted instance holding the specified value in encrypted form.
    * @param value the value to be encrypted
    * @param encrypt the `Encrypt` instance to use for encryption.
    * @tparam V the type of `value`
    */
  def apply[V : Serializer](value: V)(implicit encrypt: Encrypt): Encrypted[V] = {
    if (value == null) empty
    else Encrypted[V](encrypt(implicitly[Serializer[V]].serialize(value)))
  }
  def empty[V : Serializer]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  object Empty extends Encrypted[Nothing](CipherText.Empty) {
    override def decrypted(implicit decrypt: Decrypt) = Left("decrypted called on empty")
    override def isEmpty: Boolean = true
  }
}
