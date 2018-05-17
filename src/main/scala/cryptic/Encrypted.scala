package cryptic

import cryptic.PlainText.{Deserializer, Serializer}

import scala.language.implicitConversions

sealed abstract class Encrypted[V : Deserializer : Serializer] {
  import Encrypted._
  def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]]
  def decrypted(implicit decrypt: Decrypt): Either[String, V]
  def filter(pred: V => Boolean): Encrypted[Option[V]] = Filtered(this, pred)
  def map[W : Deserializer : Serializer](f: V => W): Encrypted[W] = Mapped(this, f)
  def flatMap[W : Deserializer : Serializer](f: V => Encrypted[W]): Encrypted[W] = FlatMapped(this, f)
}
object Encrypted {
  def empty[V]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  case object Empty extends Encrypted[Nothing] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt) = Left("run called on empty")
    override def decrypted(implicit decrypt: Decrypt) = Left("decrypt called on empty")
    override def filter(pred: Nothing => Boolean): Encrypted[Option[Nothing]] = empty
    override def map[W : Deserializer : Serializer](f: Nothing => W): Encrypted[W] = empty
    override def flatMap[W : Deserializer : Serializer](f: Nothing => Encrypted[W]): Encrypted[W] = empty
  }
  case class Value[V : Deserializer : Serializer](cipherText: CipherText) extends Encrypted[V] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt) = Right(this)
    override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
      decrypt(cipherText).flatMap(implicitly[Deserializer[V]].deserialize)
  }
  def apply[V : Serializer : Deserializer](value: V)(implicit encrypt: Encrypt): Encrypted[V] = {
    if (value == null) empty
    else Value[V](encrypt(implicitly[Serializer[V]].serialize(value)))
  }
  def apply[V : Deserializer : Serializer](cipherText: CipherText): Encrypted[V] = Value[V](cipherText)
  final case class Filtered[V : Deserializer : Serializer](e: Encrypted[V], pred: V => Boolean) extends Encrypted[Option[V]] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[Option[V]]] =
      decrypted.map(_.map(implicitly[Serializer[V]].serialize) match {
        case Some(pt) => Value(encrypt(pt))
        case None => empty
      })
    override def decrypted(implicit decrypt: Decrypt): Either[String, Option[V]] =
      e.decrypted.map(v => Option(v).filter(pred))
  }
  final case class Mapped[V, W : Deserializer : Serializer](e: Encrypted[V], f: V => W) extends Encrypted[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Value(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] = e.decrypted.map(f)
  }
  final case class FlatMapped[V, W : Deserializer : Serializer](e: Encrypted[V], f: V => Encrypted[W]) extends Encrypted[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Value(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] = e.decrypted.flatMap[String, W](v => f(v).decrypted)
  }

}