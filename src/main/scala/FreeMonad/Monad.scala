package FreeMonad

import scala.concurrent.Future

trait Monad[F[_]] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = this.flatMap(fa)(a => pure(f(a)))

}

object Monad {

  def apply[M[_]](implicit monad: Monad[M]): Monad[M] = monad

  implicit val monadFuture: Monad[Future] = new Monad[Future] {

    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      ???

    override def pure[A](a: A): Future[A] = ???

  }

}
