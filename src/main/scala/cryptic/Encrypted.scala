package cryptic

import scala.language.implicitConversions

trait Encryptor {
  def encrypt(plainText: PlainText): CipherText
}
trait Decryptor {
  def decrypt(cipherText: CipherText): Either[String, PlainText]
}
sealed abstract class Encrypted[V : PlainText.Deserializer] {
  import Encrypted._
  def run(implicit encryptor: Encryptor, decryptor: Decryptor): Either[String, Encrypted[V]]
  def decrypted(implicit decryptor: Decryptor): Either[String, V]
  def filter(pred: V => Boolean): Encrypted[Option[V]] = Filtered(this, pred)
  def map[W](f: V => W): Encrypted[W] = Mapped(this, f)
  def flatMap[W](f: V => Encrypted[W]): Encrypted[W] = FlatMapped(this, f)
}
object Encrypted {
  def empty[V]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  case object Empty extends Encrypted[Nothing] {
    override def run(implicit encryptor: Encryptor, decryptor: Decryptor) = Left("run called on empty")
    override def decrypted(implicit decryptor: Decryptor) = Left("decrypt called on empty")
    override def filter(pred: Nothing => Boolean): Encrypted[Option[Nothing]] = empty
    override def map[W](f: Nothing => W): Encrypted[W] = empty
    override def flatMap[W](f: Nothing => Encrypted[W]): Encrypted[W] = empty
  }
  case class Value[V : PlainText.Deserializer](cipherText: CipherText) extends Encrypted[V] {
    override def run(implicit encryptor: Encryptor, decryptor: Decryptor) = Right(this)
    override def decrypted(implicit decryptor: Decryptor): Either[String, V] =
      decryptor.decrypt(cipherText).flatMap(implicitly[PlainText.Deserializer[V]].deserialize)
  }
  def apply[V : PlainText.Serializer : PlainText.Deserializer](value: V)(implicit encryptor: Encryptor): Encrypted[V] = {
    if (value == null) empty
    else Value[V](encryptor.encrypt(implicitly[PlainText.Serializer[V]].serialize(value)))
  }
  final case class Filtered[V : PlainText.Deserializer](e: Encrypted[V], pred: V => Boolean) extends Encrypted[Option[V]] {
    override def run(implicit encryptor: Encryptor, decryptor: Decryptor): Either[String, Encrypted[Option[V]]] =
      decrypted.map(_.map(implicitly[PlainText.Serializer[V]].serialize) match {
        case Some(pt) => Value(encryptor.encrypt(pt))
        case None => empty
      })
    override def decrypted(implicit decryptor: Decryptor): Either[String, Option[V]] =
      e.decrypted.map(v => Option(v).filter(pred))
  }
  final case class Mapped[V, W](e: Encrypted[V], f: V => W) extends Encrypted[W] {
    override def run(implicit encryptor: Encryptor, decryptor: Decryptor): Either[String, Encrypted[W]] =
      decrypted.map(w => Value(encryptor.encrypt(implicitly[PlainText.Serializer[W]].serialize(w))))
    override def decrypted(implicit decryptor: Decryptor): Either[String, W] = e.decrypted.map(f)
  }
  final case class FlatMapped[V, W](e: Encrypted[V], f: V => Encrypted[W]) extends Encrypted[W] {
    override def run(implicit encryptor: Encryptor, decryptor: Decryptor): Either[String, Encrypted[W]] =
      decrypted.map(w => Value(encryptor.encrypt(implicitly[PlainText.Serializer[W]].serialize(w))))
    override def decrypted(implicit decryptor: Decryptor): Either[String, W] = e.decrypted.flatMap[String, W](v => f(v).decrypted)
  }

}