package cryptic.support

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

/**
 * Common test base to provide ScalaTest Matchers and a default global ExecutionContext.
 */
trait TestBase extends AnyFlatSpec with Matchers
