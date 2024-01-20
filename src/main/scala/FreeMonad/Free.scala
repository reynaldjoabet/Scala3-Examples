package FreeMonad
import Free._


trait Free[M[_], A] {

  def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))

  def foldMap[G[_]: Monad](natTrans: M ~> G): G[A] = this match {
    case Pure(a)     => Monad[G].pure(a)
    case Suspend(ma) => natTrans.apply(ma)
    case FlatMap(fa, f) => // need a G[B]
      Monad[G].flatMap(fa.foldMap(natTrans))(a => f(a).foldMap(natTrans))
  }

}

object Free {

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  def liftF[F[_],A](fa:F[A]): Free[F, A]= Suspend(fa)

  case class Pure[M[_], A](a: A) extends Free[M, A]
  case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B])
      extends Free[M, B]
  case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]


implicit def freeMonadInstance[F[_]]:Monad[[X]=>>Free[F,X]]= new Monad[[X]=>>Free[F,X]]{
override def pure[A](a: A): Free[F, A] = Free.pure(a)

override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.map(f)
override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
  }


}
