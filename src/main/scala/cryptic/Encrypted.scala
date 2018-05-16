package cryptic

final case class Secret(data: String) {
  override def productElement(n: Int) = if (n == 0) "\uD83D\uDD12" else throw new IndexOutOfBoundsException(n.toString)
}
trait Encrypter[V, K] {
  def encrypt(v: V)(implicit key: K): Secret
}
trait Decrypter[V, K] {
  def decrypt(s: Secret)(implicit key: K): Option[V]
}
sealed abstract class Encrypted[+V, K](implicit encv: Encrypter[V, K], decv: Decrypter[V , K]) {
  import Encrypted._
  def decrypted(implicit key: K): Option[V]
  def filter(pred: V => Boolean): Encrypted[V, K] = Filtered(this, pred)
  def map[W](f: V => W)(implicit encw: Encrypter[W, K], decw: Decrypter[W , K]): Encrypted[W, K] = Mapped(this, f)
  def flatMap[W](f: V => Encrypted[W, K])(implicit encw: Encrypter[W, K], decw: Decrypter[W , K]): Encrypted[W, K] = FlatMapped(this, f)
}
object Encrypted {
  def empty[V, K]: Encrypted[V, K] = Empty.asInstanceOf[Encrypted[V, K]]
  case object Empty extends Encrypted[Nothing, Any]()(Implicits.nothingEncrypter, Implicits.nothingDecrypter) {
    override def decrypted(implicit key: Any): None.type = None
    override def filter(pred: Nothing => Boolean): Encrypted[Nothing, Any] = this
    override def map[W](f: Nothing => W)(implicit encw: Encrypter[W, Any], decw: Decrypter[W , Any]): Encrypted[W, Any] = this
    override def flatMap[W](f: Nothing => Encrypted[W, Any])(implicit encw: Encrypter[W, Any], decw: Decrypter[W , Any]): Encrypted[W, Any] = this
  }
  case class Value[+V, K](s: Secret)(implicit encv: Encrypter[V, K], decv: Decrypter[V , K]) extends Encrypted[V, K] {
    override def decrypted(implicit key: K): Option[V] = decv.decrypt(s)
  }
  def apply[V, K](value: V)(implicit encv: Encrypter[V, K], decv: Decrypter[V , K], key: K): Encrypted[V, K] = {
    if (value == null) empty
    else Value(encv.encrypt(value))
  }
  //sealed trait View[V, K] extends Encrypted[V, K]
  final case class Filtered[V, K](e: Encrypted[V, K], pred: V => Boolean)(implicit encv: Encrypter[V, K], decv: Decrypter[V , K]) extends Encrypted[V, K] {
    override def decrypted(implicit key: K): Option[V] = e.decrypted.filter(pred)
  }
  final case class Mapped[V, W, K](e: Encrypted[V, K], f: V => W)(implicit encw: Encrypter[W, K], decw: Decrypter[W , K]) extends Encrypted[W, K] {
    override def decrypted(implicit key: K): Option[W] = e.decrypted.map(f)
  }
  final case class FlatMapped[V, W, K](e: Encrypted[V, K], f: V => Encrypted[W, K])(implicit encw: Encrypter[W, K], decw: Decrypter[W , K]) extends Encrypted[W, K] {
    override def decrypted(implicit key: K): Option[W] = e.decrypted match {
      case None => None
      case Some(v) => f(v).decrypted
    }
  }

  object Implicits {
    implicit def nothingEncrypter[K]: Encrypter[Nothing, K] = new Encrypter[Nothing, K] {
      override def encrypt(v: Nothing)(implicit key: K): Secret = Secret(null)
    }
    implicit def nothingDecrypter[K]: Decrypter[Nothing, K] = new Decrypter[Nothing, K] {
      override def decrypt(s: Secret)(implicit key: K): Option[Nothing] = None
    }
    implicit def stringEncrypter[K]: Encrypter[String, K] = new Encrypter[String, K] {
      override def encrypt(v: String)(implicit key: K): Secret = Secret(v)
    }
    implicit def stringDecrypter[K]: Decrypter[String, K] = new Decrypter[String, K] {
      override def decrypt(s: Secret)(implicit key: K): Option[String] = Option(s.data)
    }
  }

  object Crypto {
    object Ceasar {
      case class Key(offset: Int)
      implicit def stringEncrypter: Encrypter[String, Key] = new Encrypter[String, Key] {
        override def encrypt(v: String)(implicit key: Key): Secret = Secret(v.map(_ + key.offset).map(_.toChar).mkString)
      }
      implicit def stringDecrypter: Decrypter[String, Key] = new Decrypter[String, Key] {
        override def decrypt(s: Secret)(implicit key: Key): Option[String] = Option(s).map(_.data.map(_ - key.offset).map(_.toChar).mkString)
      }
    }
  }
}