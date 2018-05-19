package cryptic

import syntax._
import serialization.Serializer
import scala.language.implicitConversions

sealed abstract class Encrypted[V: Serializer] {
  import Encrypted._
  def decrypted(implicit decrypt: Decrypt): Either[String, V]
  def filter(pred: V => Boolean): Operation[V] = Filtered(this, pred)
  def map[W: Serializer](f: V => W): Operation[W] = Mapped(this, f)
  def flatMap[W: Serializer](f: V => Encrypted[W]): Operation[W] = FlatMapped(this, f)
}
object Encrypted {
  def apply[V: Serializer](value: V)(implicit encrypt: Encrypt): Value[V] =
    if (value == null) empty
    else Value[V](encrypt(implicitly[Serializer[V]].serialize(value)))
  def apply[V: Serializer](cipherText: CipherText): Encrypted[V] = Value[V](cipherText)
  case class Value[V: Serializer](cipherText: CipherText) extends Encrypted[V] {
    override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
      decrypt(cipherText).flatMap(implicitly[Serializer[V]].deserialize)
    def bytes: Array[Byte] = cipherText.bytes
  }
  def empty[V]: Value[V] = Empty.asInstanceOf[Value[V]]

  object Empty extends Value[Nothing](CipherText.Empty) {
    override def decrypted(implicit decrypt: Decrypt) = Left("decrypt called on empty")
  }
  sealed trait Operation[V] extends Encrypted[V] {
    def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Value[V]]
  }
  final case class Filtered[V: Serializer](src: Encrypted[V], pred: V => Boolean) extends Operation[V] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Value[V]] = {
      src.decrypted.map[ Value[V]] { v ⇒
       if (pred(v)) v.encrypted
       else empty[V]
      }
    }
    override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
      src.decrypted.flatMap(v => if (pred(v)) Right(v) else Left("No such element"))
  }
  final case class Mapped[V, W: Serializer](src: Encrypted[V], f: V => W) extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Value[W]] =
      decrypted.map(w => Value(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] = src.decrypted.map(f)
    def bytes(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Array[Byte]] = run(encrypt, decrypt).map(_.bytes)
  }
  final case class FlatMapped[V, W: Serializer](src: Encrypted[V], f: V => Encrypted[W]) extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Value[W]] =
      decrypted.map(w => Value(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.flatMap[String, W](v => f(v).decrypted)
  }
}
