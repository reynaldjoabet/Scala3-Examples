package FreeMonad

case class IO[A](unsafeRun: () => A)

object IO {
  implicit val ioMonad: Monad[IO] = new Monad[IO] {

    override def pure[A](a: A) = IO(() => a)
    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]) =
      IO(() => f(ma.unsafeRun()).unsafeRun())
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = ???

  }
}
