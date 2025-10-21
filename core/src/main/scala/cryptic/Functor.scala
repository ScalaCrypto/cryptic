package cryptic

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

/** Represents a type class for effectful computations, providing a set of
  * operations to work with effects of higher-kinded types.
  *
  * @tparam F
  *   the higher-kinded type representing the effect structure
  */
trait Functor[F[_]]:
  extension [V](v: V) def pure: F[V]
  extension [V](throwable: Throwable) def failed: F[V]
  extension [A, B](fa: F[A])
    def map(f: A => B): F[B]
    def flatMap(f: A => F[B]): F[B]
  extension [V](fv: F[V])
    def transform[G[_]: Functor]: G[V]
    def foreach(f: V => Unit): Unit
  extension [W >: V, V](fv: F[V])
    def recoverWith(pf: PartialFunction[Throwable, F[W]]): F[W]
  extension [V](t: Try[V]) def lift: F[V] = t.fold(failed, v => v.pure)

/** Standard Functor instances for common Scala effect types (Id, Try, Future, Option, Either). */
object Functor:

  given idFunctor: Functor[Id] with
    extension [V](v: V) def pure: V = v
    extension [V](throwable: Throwable) def failed: Id[V] = throw throwable
    extension [A, B](fa: Id[A])
      def map(f: A => B): Id[B] = f(fa)
      def flatMap(f: A => Id[B]): Id[B] = f(fa)
    extension [V](v: Id[V])
      def transform[G[_]](using functor: Functor[G]): G[V] = v.pure
      def foreach(f: V => Unit): Unit = f(v)
    extension [W >: V, V](fv: Id[V])
      override def recoverWith(pf: PartialFunction[Throwable, Id[W]]): Id[W] =
        fv

  given tryFunctor: Functor[Try] with
    extension [V](v: V) def pure: Try[V] = Try(v)
    extension [V](throwable: Throwable) def failed: Try[V] = Failure(throwable)
    extension [A, B](fa: Try[A])
      def map(f: A => B): Try[B] = fa.map(f)
      def flatMap(f: A => Try[B]): Try[B] = fa.flatMap(f)
    extension [V](tv: Try[V])
      def transform[G[_]: Functor]: G[V] = tv.lift
      def foreach(f: V => Unit): Unit = tv.foreach(f)
    extension [W >: V, V](fv: Try[V])
      override def recoverWith(pf: PartialFunction[Throwable, Try[W]]): Try[W] =
        fv.recoverWith(pf)

  given futureFunctor(using ec: ExecutionContext): Functor[Future] with
    extension [V](v: V) def pure: Future[V] = Future.successful(v)
    extension [V](throwable: Throwable)
      def failed: Future[V] = Future.failed(throwable)
    extension [A, B](fa: Future[A])
      def map(f: A => B): Future[B] = fa.map(f)
      def flatMap(f: A => Future[B]): Future[B] = fa.flatMap(f)
    extension [V](fv: Future[V])
      def transform[G[_]: Functor]: G[V] =
        new UnsupportedOperationException(
          "transform on Future is not supported"
        ).failed
      def foreach(f: V => Unit): Unit = fv.foreach(f)
    extension [W >: V, V](fv: Future[V])
      override def recoverWith(
          pf: PartialFunction[Throwable, Future[W]]
      ): Future[W] = fv.recoverWith(pf)

  given optionFunctor: Functor[Option] with
    extension [V](v: V) def pure: Option[V] = Option(v)
    extension [V](throwable: Throwable) def failed: Option[V] = None
    extension [A, B](fa: Option[A])
      def map(f: A => B): Option[B] = fa.map(f)
      def flatMap(f: A => Option[B]): Option[B] = fa.flatMap(f)
    extension [V](ov: Option[V])
      def transform[G[_]: Functor]: G[V] =
        ov match
          case Some(v) => v.pure
          case None    => new NoSuchElementException("empty Option").failed
      def foreach(f: V => Unit): Unit = ov.foreach(f)
    extension [W >: V, V](fv: Option[V])
      override def recoverWith(
          pf: PartialFunction[Throwable, Option[W]]
      ): Option[W] =
        fv match
          case s @ Some(_) => s
          case None =>
            val ex = new NoSuchElementException("empty Option")
            if pf.isDefinedAt(ex) then pf(ex) else None

  given eitherFunctor: Functor[[x] =>> Either[Throwable, x]] with
    extension [V](v: V) def pure: Either[Throwable, V] = Right(v)
    extension [V](throwable: Throwable)
      def failed: Either[Throwable, V] = Left(throwable)
    extension [A, B](fa: Either[Throwable, A])
      def map(f: A => B): Either[Throwable, B] = fa.map(f)
      def flatMap(f: A => Either[Throwable, B]): Either[Throwable, B] =
        fa.flatMap(f)
    extension [V](ev: Either[Throwable, V])
      def transform[G[_]: Functor]: G[V] =
        ev match
          case Right(v) => v.pure
          case Left(ex) => ex.failed
      def foreach(f: V => Unit): Unit = ev.foreach(f)
    extension [W >: V, V](fv: Either[Throwable, V])
      override def recoverWith(
          pf: PartialFunction[Throwable, Either[Throwable, W]]
      ): Either[Throwable, W] =
        fv match
          case r @ Right(_) => r
          case Left(ex)     => if pf.isDefinedAt(ex) then pf(ex) else Left(ex)
