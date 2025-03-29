package cryptic
package test

import cryptic.{Cryptic, Encrypted}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{PrivateKey, PublicKey}
import scala.util.{Success, Try}

// #data-types

case class EmailAddress(literal: String)
case class User(id: Long, email: Encrypted[EmailAddress])
// #data-types

class GettingStartedSpec extends AnyFlatSpec with Matchers:

  "Getting started guide" should "work" in:
    for i <- 0 to 0 do {
      // #import
    }
    val key =
      // #generate-key
      import cryptic.crypto.RSA.*
      val key = keygen(2048)
      // #generate-key
      key

    val (publicKey, privateKey) =
      // #extract-key
      implicit val publicKey: PublicKey = key.getPublic
      implicit val privateKey: PrivateKey = key.getPrivate
      // #extract-key
      (publicKey, privateKey)

    val user: User =
      import cryptic.crypto.RSA.*
      import cryptic.crypto.RSA.given
      import cryptic.serialization.Chill.*
      import cryptic.serialization.Chill.given
      import cryptic.syntax.*
      implicit val pubKey: PublicKey = publicKey
      // #encrypt
      val user = User(123, EmailAddress("Odd@Example.com").encrypted)
      // #encrypt
      user

    // #access
    val bytes: Array[Byte] = user.email.bytes
    // #access

    val loweredEmailOp: Cryptic.Operation[EmailAddress] =
      // # transform
      import Cryptic.*
      import cryptic.serialization.Chill.*
      import cryptic.serialization.Chill.given
      val loweredEmailOp: Operation[EmailAddress] =
        user.email.map(email => email.copy(literal = email.literal.toLowerCase))
      // # transform
      loweredEmailOp
    val userWithLoweredEmail: Try[User] =
      // #run
      import cryptic.crypto.RSA.*
      import cryptic.crypto.RSA.given
      implicit val publicKey: PublicKey = key.getPublic
      implicit val privateKey: PrivateKey = key.getPrivate
      val userWithLoweredEmail: Try[User] =
        loweredEmailOp.run.map(email => user.copy(email = email))
      // #run
      userWithLoweredEmail
    val loweredEmail =
      // #decrypt
      import cryptic.crypto.RSA.*
      import cryptic.crypto.RSA.given
      implicit val privateKey: PrivateKey = key.getPrivate
      val loweredEmail: Try[EmailAddress] =
        userWithLoweredEmail.flatMap(_.email.decrypted)
      // #decrypt
      loweredEmail

    loweredEmail shouldEqual Success(EmailAddress("odd@example.com"))
