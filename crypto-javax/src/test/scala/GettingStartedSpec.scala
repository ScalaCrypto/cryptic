package cryptic
package crypto

import org.scalatest._

// #data-types
case class EmailAddress(literal: String)
case class User(id: Long, email: Encrypted[EmailAddress])
// #data-types

class GettingStartedSpec extends FlatSpec with Matchers {

  "Getting started guide" should "work" in {
    {
      // #import-1
      import cryptic._
      import cryptic.syntax._
      // #import-1
    }

    val key = {
      // #generate-key
      import cryptic.syntax._
      import cryptic.crypto.RSA._
      import cryptic.serialization.Fst._
      val key = keygen(2048)
      // #generate-key
      key
    }

    val (publicKey, privateKey) = {
      // #extract-key
      implicit val publicKey = key.getPublic
      implicit val privateKey = key.getPrivate
      // #extract-key
      (publicKey, privateKey)
    }

    val user: User = {
      import cryptic.syntax._
      import cryptic.crypto.RSA._
      import cryptic.serialization.Fst._
      implicit val pubKey = publicKey
      // #encrypt
      val user = User(123, EmailAddress("Odd@Example.com").encrypted)
      // #encrypt
      user
    }

    // #access
    val bytes: Array[Byte] = user.email.bytes
    // #access

    val loweredEmailOp: Cryptic.Operation[EmailAddress] = {
      // # transform
      import Cryptic._
      import cryptic.serialization.Fst._
      val loweredEmailOp: Operation[EmailAddress] = user.email.
        map(email => email.copy(literal = email.literal.toLowerCase))
      // # transform
      loweredEmailOp
    }
    val userWithLoweredEmail: Either[String, User] = {
      // #run
      import cryptic.crypto.RSA._
      import cryptic.serialization.Fst._
      implicit val publicKey = key.getPublic
      implicit val privateKey = key.getPrivate
      val userWithLoweredEmail: Either[String, User] = loweredEmailOp.run.map(email => user.copy(email = email))
      // #run
      userWithLoweredEmail
    }
    val loweredEmail = {
      // #decrypt
      import cryptic.crypto.RSA._
      import cryptic.serialization.Fst._
      implicit val privateKey = key.getPrivate
      val loweredEmail: Either[String, EmailAddress] = userWithLoweredEmail.flatMap(_.email.decrypted)
      // #decrypt
      loweredEmail
    }

    loweredEmail shouldEqual Right(EmailAddress("odd@example.com"))
  }
}
