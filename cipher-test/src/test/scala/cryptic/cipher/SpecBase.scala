package cryptic.cipher

import cryptic.*
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.compiletime.deferred
import scala.util.Try

trait SpecBase extends AnyFlatSpecLike with Matchers with TryValues:
  given stringCodec: Codec[String] = deferred
  given functor: Functor[Try] = Functor.tryFunctor
