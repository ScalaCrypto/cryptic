package cryptic
package support

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

/** Common test base to provide ScalaTest Matchers and a default global
  * ExecutionContext.
  */
trait AsyncTestBase extends AsyncFlatSpec with ScalaFutures with Matchers:
  given ec: ExecutionContext = ExecutionContext.global
