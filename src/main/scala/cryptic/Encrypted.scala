package cryptic

final case class CipherText(data: String) {
  override def productElement(n: Int) = if (n == 0) "\uD83D\uDD12" else throw new IndexOutOfBoundsException(n.toString)
}
trait Encryptor[V] {
  def encrypt(value: V): CipherText
}
trait Decryptor[V] {
  def decrypt(cipherText: CipherText): Option[V]
}
sealed trait Encrypted[V] {
  import Encrypted._
  def decrypted(implicit decryptor: Decryptor[V]): Option[V]
  def filter(pred: V => Boolean): Encrypted[V] = Filtered(this, pred)
  def map[W](f: V => W): Encrypted[W] = Mapped(this, f)
  def flatMap[W](f: V => Encrypted[W]): Encrypted[W] = FlatMapped(this, f)
}
object Encrypted {
  def empty[V]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  case object Empty extends Encrypted[Nothing]{
    override def decrypted(implicit decryptor: Decryptor[Nothing]): None.type = None
    override def filter(pred: Nothing => Boolean): Encrypted[Nothing] = this
    override def map[W](f: Nothing => W): Encrypted[W] = empty
    override def flatMap[W](f: Nothing => Encrypted[W]): Encrypted[W] = empty
  }
  case class Value[V](s: CipherText) extends Encrypted[V] {
    override def decrypted(implicit decryptor: Decryptor[V]): Option[V] = decryptor.decrypt(s)
  }
  def apply[V](value: V)(implicit encryptor: Encryptor[V]): Encrypted[V] = {
    if (value == null) empty
    else Value(encryptor.encrypt(value))
  }
  //sealed trait View[V] extends Encrypted[V]
  final case class Filtered[V](e: Encrypted[V], pred: V => Boolean) extends Encrypted[V] {
    override def decrypted(implicit decryptor: Decryptor[V]): Option[V] = e.decrypted.filter(pred)
  }
  final case class Mapped[V, W](e: Encrypted[V], f: V => W) extends Encrypted[W] {
    override def decrypted(implicit decryptor: Decryptor[V]): Option[W] = e.decrypted.map(f)
  }
  final case class FlatMapped[V, W](e: Encrypted[V], f: V => Encrypted[W]) extends Encrypted[W] {
    override def decrypted(implicit decryptor: Decryptor[W]): Option[W] = e.decrypted match {
      case None => None
      case Some(v) => f(v).decrypted
    }
  }

  object Implicits {
    implicit val nothingEncryptor: Encryptor[Nothing] = new Encryptor[Nothing] {
      override def encrypt(v: Nothing): CipherText = CipherText(null)
    }
    implicit val nothingDecryptor: Decryptor[Nothing] = new Decryptor[Nothing] {
      override def decrypt(s: CipherText): Option[Nothing] = None
    }
    implicit val stringEncryptor: Encryptor[String] = new Encryptor[String] {
      override def encrypt(v: String): CipherText = CipherText(v)
    }
    implicit val stringDecryptor: Decryptor[String] = new Decryptor[String] {
      override def decrypt(s: CipherText): Option[String] = Option(s.data)
    }
  }

  object Crypto {
    object Ceasar {
      case class Key(offset:Int)
      implicit def stringEncryptor(key:Key): Encryptor[String] = new Encryptor[String] {
        override def encrypt(v: String): CipherText = CipherText(v.map(_ + key.offset).map(_.toChar).mkString)
      }
      implicit def stringDecryptor(key:Key): Decryptor[String] = new Decryptor[String] {
        override def decrypt(s: CipherText): Option[String] = Option(s).map(_.data.map(_ - key.offset).map(_.toChar).mkString)
      }
    }
  }
}