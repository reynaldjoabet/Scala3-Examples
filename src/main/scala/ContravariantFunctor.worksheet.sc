trait ContravariantFunctor[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

def function1Contravariant[C] = new ContravariantFunctor[[X] =>> Function1[X, C]] {
  override def contramap[A, B](fa: A => C)(f: B => A): B => C = fa.compose(f)

}

val length: String => Int    = _.length
val double: Double => String = _.toString

val f1: Double => Int = function1Contravariant[Int].contramap(length)(double)

//contramap is used to transform a function length: String => Int to a new function transformed: Double => Int by "pre-applying" the double: Double => String function to the input of length.
//The compose method is used to achieve this composition.

f1(3)
8.toDouble.toString().length()
8.toDouble

object types {
  opaque type Year = Int
  val year: Year = 1999

  object Year {
    def apply(value: Int): Year = value

    // to extract the value from an opaque type,add an extension method to the companion object of our opaque type
    extension (year: Year) {
      def value: Int = year
    }
  }
}

//Unlike built-in types, opaque types don’t have apply methods. Also, they don’t expose any methods of the original type
//In our example, even though Year is an opaque type alias for Int, none of the methods or operators of Int are accessible.
import types._
//But the same statement outside the object types will not compile.
//val year: Year = 1999 not compile

val year: Year = Year(2000)

import java.time.LocalDate
import java.time.LocalTime

LocalDate.now()

"20231126".take(4)

"20231126".subSequence(0, 4)
"02".toInt

"20231126".subSequence(4, 6)

"20231126".subSequence(6, 8)

val m = "20069094,20:49:00,20:49:00,1073,2,,1,0,1.9370".split(",").toList

m.length

m(8)

"5".split(",").toList
