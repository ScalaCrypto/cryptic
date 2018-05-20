package cryptic

import cryptic.serialization.Serializer
import cryptic.syntax._

import scala.language.implicitConversions

sealed abstract class Cryptic[V : Serializer] {
  import Cryptic._
  def decrypted(implicit decrypt: Decrypt): Either[String, V]
  def filter(f: V => Boolean): Operation[V] = Filtered(this, f)
  def map[W : Serializer](f: V => W): Operation[W] = Mapped(this, f)
  def flatMap[W : Serializer](f: V => Cryptic[W]): Operation[W] = FlatMapped(this, f)
}
object Cryptic {
  import Encrypted._
  sealed abstract class Operation[V : Serializer] extends Cryptic[V] {
    def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[V]]
  }
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
  final case class Mapped[V : Serializer, W : Serializer](src: Cryptic[V], f: V => W) extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Encrypted(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.map(f)
    def bytes(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Array[Byte]] =
      run.map(_.bytes)
  }
  final case class FlatMapped[V : Serializer, W : Serializer](src: Cryptic[V], f: V => Cryptic[W])
      extends Operation[W] {
    override def run(implicit encrypt: Encrypt, decrypt: Decrypt): Either[String, Encrypted[W]] =
      decrypted.map(w => Encrypted(encrypt(implicitly[Serializer[W]].serialize(w))))
    override def decrypted(implicit decrypt: Decrypt): Either[String, W] =
      src.decrypted.flatMap[String, W](v => f(v).decrypted)
  }
}
case class Encrypted[V : Serializer](cipherText: CipherText) extends Cryptic[V] {
  override def decrypted(implicit decrypt: Decrypt): Either[String, V] =
    decrypt(cipherText).flatMap(implicitly[Serializer[V]].deserialize)
  def bytes: Array[Byte] = cipherText.bytes
}
object Encrypted {
  def apply[V : Serializer](value: V)(implicit encrypt: Encrypt): Encrypted[V] =
    if (value == null) empty
    else Encrypted[V](encrypt(implicitly[Serializer[V]].serialize(value)))
  def empty[V : Serializer]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  object Empty extends Encrypted[Nothing](CipherText.Empty) {
    override def decrypted(implicit decrypt: Decrypt) = Left("decrypted called on empty")
  }
}
