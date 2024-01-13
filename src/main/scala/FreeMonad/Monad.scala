package FreeMonad

trait Monad[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def pure[A](a: A): F[A]
  def map[A,B](fa:F[A])(f:A=>B):F[B]= this.flatMap(fa)(a=>pure(f(a)))
}

object Monad {
  def apply[M[_]](implicit monad: Monad[M]): Monad[M] = monad
}
