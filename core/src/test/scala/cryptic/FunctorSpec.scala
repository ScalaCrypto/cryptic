package cryptic

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

class FunctorIdTrySpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.Id

  "Id Functor" should "support pure, map, flatMap" in:
    import Functor.idFunctor
    val a: Id[Int] = 2.pure
    a shouldEqual 2
    a.map(_ + 1) shouldEqual 3
    a.flatMap(x => (x * 2): Id[Int]) shouldEqual 4

  it should "support transform to Try and no-op recoverWith" in:
    import Functor.{idFunctor, tryFunctor}
    val a: Id[Int] = 42
    val t: Try[Int] = a.transform[Try]
    t shouldBe Success(42)
    // recoverWith on Id should just return the same value
    a.recoverWith { case _: Throwable => 0 }.asInstanceOf[Int] shouldBe 42

  it should "provide failed and lift from Try" in:
    import Functor.idFunctor
    intercept[RuntimeException]:
      val _ = new RuntimeException("boom").failed: Id[Int]
    val idFromSuccess: Id[Int] = Success(5).lift
    idFromSuccess shouldBe 5
    intercept[IllegalStateException]:
      val _ = (Failure(new IllegalStateException("bad")): Try[Int]).lift: Id[Int]

  "Try Functor" should "support pure, failed, map, flatMap" in:
    import Functor.tryFunctor
    val t1: Try[Int] = 3.pure
    t1 shouldBe Success(3)
    val ex = new RuntimeException("x")
    val t2: Try[Int] = ex.failed
    t2.isFailure shouldBe true
    t1.map(_ + 1) shouldBe Success(4)
    t1.flatMap(x => Success(x * 2)) shouldBe Success(6)

  it should "lift Try into Id and support recoverWith" in:
    // Use Id functor in scope to lift a Try into Id
    import Functor.idFunctor
    val ok: Try[Int] = Success(10)
    ok.lift shouldBe 10
    val ex = new RuntimeException("y")
    val bad: Try[Int] = Failure(ex)
    intercept[RuntimeException]:
      val _ = bad.lift: Id[Int]
    // RecoverWith uses Try functor
    import Functor.tryFunctor
    bad.recoverWith { case _: Throwable => Success(99) } shouldBe Success(99)

  it should "lift Try via Try functor (identity semantics)" in:
    import Functor.tryFunctor
    val s: Try[Int] = Success(7)
    s.lift shouldBe Success(7)
    val f: Try[Int] = Failure(new IllegalArgumentException("nope"))
    f.lift.failure.exception.getMessage shouldBe "nope"

class FunctorFutureSpec
    extends AsyncFlatSpec
    with Matchers
    with ScalaFutures:

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
    t.failed.get.getMessage should include ("transform on Future is not supported")

  it should "lift Try into Future" in:
    import Functor.futureFunctor
    Success(9).lift.map(_ shouldBe 9)
    val ex = new RuntimeException("ouch")
    Failure[Int](ex).lift
      .failed
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
    val noneInt: Option[Int] = (new RuntimeException("x")).failed
    noneInt shouldBe None
    noneInt.recoverWith { case _: Throwable => Some(42) } shouldBe Some(42)

  it should "transform to Try (None -> Failure)" in:
    import Functor.{optionFunctor, tryFunctor}
    val t1: Try[Int] = Some(7).transform[Try]
    t1 shouldBe Success(7)
    val t2: Try[Int] = (None: Option[Int]).transform[Try]
    t2.isFailure shouldBe true
    t2.failed.get shouldBe a[NoSuchElementException]
    t2.failed.get.getMessage should include ("empty Option")

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
    val t2: Try[Int] = (Left(new RuntimeException("oops")): E[Int]).transform[Try]
    t2.isFailure shouldBe true
    t2.failed.get.getMessage should include ("oops")

  it should "lift Try into Either" in:
    import Functor.eitherFunctor
    Success(3).lift shouldBe Right(3)
    val ex = new IllegalArgumentException("nope2")
    Failure[Int](ex).lift shouldBe Left(ex)
