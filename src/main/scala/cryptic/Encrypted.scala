package cryptic

import scala.language.implicitConversions

trait Encryptor {
  def encrypt(plainText: PlainText): CipherText
}
trait Decryptor {
  def decrypt(cipherText: CipherText): Either[String, PlainText]
}
sealed trait Encrypted[V] {
  import Encrypted._
  def run: Value[V]
  def decrypted(implicit decryptor: Decryptor): Either[String, V]
  def filter(pred: V => Boolean): Encrypted[Option[V]] = Filtered(this, pred)
  def map[W](f: V => W): Encrypted[W] = Mapped(this, f)
  def flatMap[W](f: V => Encrypted[W]): Encrypted[W] = FlatMapped(this, f)
}
object Encrypted {
  def empty[V]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  case object Empty extends Encrypted[Nothing] {
    override def decrypted(implicit decryptor: Decryptor) = Left("decrypt called on empty")
    override def filter(pred: Nothing => Boolean): Encrypted[Option[Nothing]] = empty
    override def map[W](f: Nothing => W): Encrypted[W] = empty
    override def flatMap[W](f: Nothing => Encrypted[W]): Encrypted[W] = empty
  }
  case class Value[V : PlainText.Decoder](cipherText: CipherText) extends Encrypted[V] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, V] =
      decryptor.decrypt(cipherText).flatMap(implicitly[PlainText.Decoder[V]].decode)

    override def run: Value[V] = this
  }
  def apply[V : PlainText.Encoder : PlainText.Decoder](value: V)(implicit encryptor: Encryptor): Encrypted[V] = {
    if (value == null) empty
    else Value[V](encryptor.encrypt(implicitly[PlainText.Encoder[V]].encode(value)))
  }
  final case class Filtered[V](e: Encrypted[V], pred: V => Boolean) extends Encrypted[Option[V]] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, Option[V]] = e.decrypted.map(v => Option(v).filter(pred))

    override def run: Value[Option[V]] = ???
  }
  final case class Mapped[V, W](e: Encrypted[V], f: V => W) extends Encrypted[W] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, W] = e.decrypted.map(f)
  }
  final case class FlatMapped[V, W](e: Encrypted[V], f: V => Encrypted[W]) extends Encrypted[W] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, W] = e.decrypted.flatMap[String, W](v => f(v).decrypted)
  }

}