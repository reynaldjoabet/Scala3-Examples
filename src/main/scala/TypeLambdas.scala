import scala.deriving.Mirror
import scala.compiletime._
object TypeLambdas {
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  type MyList = [X] =>> List[X] // type constructor
  val b: MyList[Int] = List.apply[Int](8)
  type myMap = [X] =>> Map[String, X]

  sealed abstract class Kleisli[F[_], -A, B]() {
    def run: A => F[B]
  }

  class OptionT[F[_], A](opt: F[Option[A]])

  class KleisliMonad[F[_], C] extends Monad[[X] =>> Kleisli[F, X, C]] {
    override def flatMap[A, B](fa: Kleisli[F, A, C])(
        f: A => Kleisli[F, B, C]
    ): Kleisli[F, B, C] = ???
    override def pure[A](a: A): Kleisli[F, A, C] = ???
  }
  class Function1Monad[F]     extends Monad[[X] =>> Function1[F, X]]  {
    override def flatMap[A, B](fa: F => A)(f: A => F => B): F => B = ???
    override def pure[A](a: A): F => A                             = ???
  }

  class MonadFunction2[X, Y] extends Monad[[C] =>> Function2[Int, Int, C]] {
    override def flatMap[A, B](fa: (Int, Int) => A)(
        f: A => (Int, Int) => B
    ): (Int, Int) => B = ???
    override def pure[A](a: A): (Int, Int) => A = ???
  }
  class OptionTMonad[F[_]]   extends Monad[[X] =>> OptionT[F, X]]          {
    override def flatMap[A, B](fa: OptionT[F, A])(
        f: A => OptionT[F, B]
    ): OptionT[F, B] = ???
    override def pure[A](a: A): OptionT[F, A] = ???
  }
  class EitherMonad[E]       extends Monad[[X] =>> Either[E, X]]           {
    override def flatMap[A, B](fa: Either[E, A])(
        f: A => Either[E, B]
    ): Either[E, B] = ???
    override def pure[A](a: A): Either[E, A] = ???
  }

  sealed trait HList extends Product with Serializable {
    def prepend[H](h: H): ::[H, HList] = new ::(h, this)
    def ::[H](h: H): H :: HList        = new ::(h, this)
  }

  final case class ::[+H, +T <: HList](head: H, tail: T) extends HList {}

  sealed trait HNil extends HList {}

  case object HNil extends HNil

  val h = 5 :: HNil

  val p = HNil.prepend(6)

  case class Student[A](name: String, a: A)
  val st: Student[Int] = Student("paul", 23)

  transparent inline def sum(a: Int, b: Int) = a + b
  val total: 30                              = sum(10, 20) // The type is Literal Value 30 instead of Int

  // Shapeless makes it convenient to convert specific types into generic ones that we can manipulate with common code
}
