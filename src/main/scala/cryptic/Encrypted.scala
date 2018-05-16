package cryptic

import scala.language.implicitConversions

final case class PlainText(data: String) {
  override def productElement(n: Int) = if (n == 0) "\uD83D\uDD12" else throw new IndexOutOfBoundsException(n.toString)
}
trait Encryptor {
  def encrypt(plainText: PlainText): CipherText
}
trait Decryptor {
  def decrypt(cipherText: CipherText): Either[String, PlainText]
}
trait Encoder[V] {
  def encode(value: V): PlainText
}
trait Decoder[V] {
  def decode(plainText: PlainText): Either[String, V]
}
sealed trait Encrypted[V] {
  import Encrypted._
  def decrypted(implicit decryptor: Decryptor): Either[String, V]
  def filter(pred: V => Boolean): Encrypted[Option[V]] = Filtered(this, pred)
  def map[W](f: V => W): Encrypted[W] = Mapped(this, f)
  def flatMap[W](f: V => Encrypted[W]): Encrypted[W] = FlatMapped(this, f)
}
object Encrypted {
  def empty[V]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  case object Empty extends Encrypted[Nothing]{
    override def decrypted(implicit decryptor: Decryptor) = Left("decrypt called on empty")
    override def filter(pred: Nothing => Boolean): Encrypted[Option[Nothing]] = empty
    override def map[W](f: Nothing => W): Encrypted[W] = empty
    override def flatMap[W](f: Nothing => Encrypted[W]): Encrypted[W] = empty
  }
  case class Value[V : Decoder](ct: CipherText) extends Encrypted[V] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, V] = decryptor.decrypt(ct).flatMap(implicitly[Decoder[V]].decode)
  }
  def apply[V : Encoder : Decoder](value: V)(implicit encryptor: Encryptor): Encrypted[V] = {
    if (value == null) empty
    else Value[V](encryptor.encrypt(implicitly[Encoder[V]].encode(value)))
  }
  final case class Filtered[V](e: Encrypted[V], pred: V => Boolean) extends Encrypted[Option[V]] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, Option[V]] = e.decrypted.map(v => Option(v).filter(pred))
  }
  final case class Mapped[V, W](e: Encrypted[V], f: V => W) extends Encrypted[W] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, W] = e.decrypted.map(f)
  }
  final case class FlatMapped[V, W](e: Encrypted[V], f: V => Encrypted[W]) extends Encrypted[W] {
    override def decrypted(implicit decryptor: Decryptor): Either[String, W] = e.decrypted.flatMap[String, W](v => f(v).decrypted)
  }

  object Coders {
    implicit val nothingEncoder: Encoder[Nothing] = (value: Nothing) => PlainText(null)
    implicit val nothingDecoder: Decoder[Nothing] = (plainText: PlainText) => throw new UnsupportedOperationException("decode empty")
    implicit val stringEncoder: Encoder[String] = (value: String) => PlainText(value)
    implicit val stringDecoder: Decoder[String] = (plainText: PlainText) => Right(plainText.data)
  }

  object Cryptos {
    object Ceasar {
      case class Key(offset:Int)
      implicit def encryptor(key: Key): Encryptor = (plainText: PlainText) => CipherText(plainText.data.map(_ + key.offset).map(_.toChar).mkString)
      implicit def decryptor(key: Key): Decryptor = (cipherText: CipherText) => Right[String, PlainText](PlainText(cipherText.map(ch => (ch - key.offset).toChar).mkString))
    }
  }
}