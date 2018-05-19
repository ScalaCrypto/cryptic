package cryptic

import cryptic.serialization.Serializer
import cryptic.syntax._

import scala.language.implicitConversions

sealed abstract class Encrypting[V: Serializer] {
  import Encrypting._
  def decrypted(implicit decrypt: Decrypt): Either[String, V]
  def filter(pred: V => Boolean): Operation[V] = Filtered(this, pred)
  def map[W: Serializer](f: V => W): Operation[W] = Mapped(this, f)
  def flatMap[W: Serializer](f: V => Encrypting[W]): Operation[W] = FlatMapped(this, f)
}
object Encrypting {
  import Encrypted._
  sealed trait Operation[V] extends Encrypting[V] {
    def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]]
  }
  final case class Filtered[V: Serializer](src: Encrypting[V], pred: V => Boolean) extends Operation[V] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]] = {
      src.decrypted.map[ Encrypted[V]] { v ⇒
       if (pred(v)) v.encrypted
       else empty[V]
      }
    }
    override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
      src.decrypted.flatMap(v => if (pred(v)) Right(v) else Left("No such element"))
  }
  final case class Mapped[V, W: Serializer](src: Encrypting[V], f: V => W) extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Encrypted(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] = src.decrypted.map(f)
    def bytes(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Array[Byte]] = run(encrypt, decrypt).map(_.bytes)
  }
  final case class FlatMapped[V, W: Serializer](src: Encrypting[V], f: V => Encrypting[W]) extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Encrypted(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.flatMap[String, W](v => f(v).decrypted)
  }
}
case class Encrypted[V: Serializer](cipherText: CipherText) extends Encrypting[V] {
  override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
    decrypt(cipherText).flatMap(implicitly[Serializer[V]].deserialize)
  def bytes: Array[Byte] = cipherText.bytes
}
object Encrypted {
  def apply[V: Serializer](value: V)(implicit encrypt: Encrypt): Encrypted[V] =
    if (value == null) empty
    else Encrypted[V](encrypt(implicitly[Serializer[V]].serialize(value)))
  def apply[V: Serializer](cipherText: CipherText): Encrypted[V] = Encrypted[V](cipherText)
  def empty[V]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  object Empty extends Encrypted[Nothing](CipherText.Empty) {
    override def decrypted(implicit decrypt: Decrypt) = Left("decrypt called on empty")
  }
}

