package cryptic

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

class FunctorFutureSpec extends AsyncFlatSpec with Matchers with ScalaFutures:

  given ExecutionContext = ExecutionContext.global

  "Future Functor" should "support pure, map, flatMap" in:
    import Functor.futureFunctor
    val fa: Future[Int] = 4.pure
    fa.map(_ + 1).map(_ shouldBe 5)

  it should "support failed and recoverWith" in:
    import Functor.futureFunctor
    val ex = new IllegalStateException("boom")
    val fb: Future[Int] = ex.failed
    fb.recoverWith { case _: Throwable => Future.successful(123) }
      .map(_ shouldBe 123)

  it should "not support transform (returns Failure in target functor)" in:
    import Functor.{futureFunctor, tryFunctor}
    val t: Try[Int] = Future.successful(1).transform[Try]
    t.isFailure shouldBe true
    t.failed.get shouldBe a[UnsupportedOperationException]
    t.failed.get.getMessage should include(
      "transform on Future is not supported"
    )

  it should "lift Try into Future" in:
    import Functor.futureFunctor
    Success(9).lift.map(_ shouldBe 9)
    val ex = new RuntimeException("ouch")
    Failure[Int](ex).lift.failed
      .map(th => th.getMessage shouldBe "ouch")

class FunctorOptionSpec extends AnyFlatSpec with Matchers with TryValues:

  "Option Functor" should "support pure, map, flatMap" in:
    import Functor.optionFunctor
    val o1: Option[Int] = 5.pure
    o1 shouldBe Some(5)
    o1.map(_ + 1) shouldBe Some(6)
    o1.flatMap(x => Some(x * 2)) shouldBe Some(10)

  it should "support failed and recoverWith" in:
    import Functor.optionFunctor
    val noneInt: Option[Int] = new RuntimeException("x").failed
    noneInt shouldBe None
    noneInt.recoverWith { case _: Throwable => Some(42) } shouldBe Some(42)

  it should "transform to Try (None -> Failure)" in:
    import Functor.{optionFunctor, tryFunctor}
    val t1: Try[Int] = Some(7).transform[Try]
    t1 shouldBe Success(7)
    val t2: Try[Int] = (None: Option[Int]).transform[Try]
    t2.isFailure shouldBe true
    t2.failed.get shouldBe a[NoSuchElementException]
    t2.failed.get.getMessage should include("empty Option")

  it should "lift Try into Option" in:
    import Functor.optionFunctor
    Success(3).lift shouldBe Some(3)
    val ex = new IllegalArgumentException("nope")
    Failure[Int](ex).lift shouldBe None

class FunctorEitherSpec extends AnyFlatSpec with Matchers with TryValues:

  type E[A] = Either[Throwable, A]

  "Either Functor" should "support pure, map, flatMap" in:
    import Functor.eitherFunctor
    val e1: E[Int] = 5.pure
    e1 shouldBe Right(5)
    e1.map(_ + 1) shouldBe Right(6)
    e1.flatMap(x => Right(x * 2)) shouldBe Right(10)

  it should "support failed and recoverWith" in:
    import Functor.eitherFunctor
    val ex = new IllegalArgumentException("bad")
    val leftInt: E[Int] = ex.failed
    leftInt shouldBe Left(ex)
    leftInt.recoverWith { case _: Throwable => Right(42) } shouldBe Right(42)

  it should "transform to Try (Left -> Failure)" in:
    import Functor.{eitherFunctor, tryFunctor}
    val t1: Try[Int] = (Right(7): E[Int]).transform[Try]
    t1 shouldBe Success(7)
    val t2: Try[Int] =
      (Left(new RuntimeException("oops")): E[Int]).transform[Try]
    t2.isFailure shouldBe true
    t2.failed.get.getMessage should include("oops")

  it should "lift Try into Either" in:
    import Functor.eitherFunctor
    Success(3).lift shouldBe Right(3)
    val ex = new IllegalArgumentException("nope2")
    Failure[Int](ex).lift shouldBe Left(ex)
