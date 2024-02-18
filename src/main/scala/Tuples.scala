import scala.deriving.Mirror

import scala.compiletime._
import scala.deriving.Mirror.Sum
import scala.deriving.*
import scala.collection.AbstractIterable

object Others {
  // @main
  def mainApp =
    "".sayHi
  val k       = 2 :: 3 :: Nil

  val g = Nil.::(3)
  val f = List(3)
  extension (s: String) {
    def sayHi = println("thello dude")
  }

  object Tuples {
    def to[A <: Product](value: A)(using
        mirror: Mirror.ProductOf[A]
    ): mirror.MirroredElemTypes = Tuple.fromProductTyped(value)

    def from[A](value: Product)(using
        mirror: Mirror.ProductOf[A],
        ev: value.type <:< mirror.MirroredElemTypes
    ): A = mirror.fromProduct(value)
  }

  final case class Vehicle(manufacturer: String, wheels: Int)

  Tuples.to(Vehicle(manufacturer = "Lada", wheels = 4))
// ("Lada", 4)
  Tuples.from[Vehicle](("Simson", 2))
// Vehicle("Simson", 2)

  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {

    import scala.deriving.Mirror

    def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T]         =
      new Eq[T] {
        def eqv(x: T, y: T): Boolean = {
          val ordx = s.ordinal(x) // (3)
          (s.ordinal(y) == ordx) && check(x, y, elems(ordx)) // (4)
        }
      }
    def check(x: Any, y: Any, elem: Eq[?]): Boolean                        =
      elem.asInstanceOf[Eq[Any]].eqv(x, y)
    def iterable[T](p: T): Iterable[Any]                                   = new AbstractIterable[Any] {
      def iterator: Iterator[Any] = p.asInstanceOf[Product].productIterator

    }
    def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
      new Eq[T] {
        def eqv(x: T, y: T): Boolean =
          iterable(x).lazyZip(iterable(y)).lazyZip(elems).forall(check)
      }

    inline def summonInstances[T, Elems <: Tuple]: List[Eq[?]] =
      inline erasedValue[Elems] match {
        case _: (elem *: elems) =>
          deriveOrSummon[T, elem] :: summonInstances[T, elems]
        case _: EmptyTuple      => Nil
      }
    inline def deriveOrSummon[T, Elem]: Eq[Elem]               =
      inline erasedValue[Elem] match {
        case _: T => deriveRec[T, Elem]
        case _    => summonInline[Eq[Elem]]
      }
    inline def deriveRec[T, Elem]: Eq[Elem]                    =
      inline erasedValue[T] match {
        case _: Elem => error("infinite recursive derivation")
        case _       =>
          Eq.derived[Elem](using
            summonInline[Mirror.Of[Elem]]
          ) // recursive derivation
      }

    inline def derived[T](using m: Mirror.Of[T]): Eq[T] = {
      lazy val elemInstances = summonInstances[T, m.MirroredElemTypes] // (1)
      inline m match { // (2)
        case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
        case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
      }
    }
  }
  def matchOnTuple(t: Tuple) = t match {
    case EmptyTuple   => None
    case head *: tail => Some(head)

  }
  (10, "x", true): (Int, String, Boolean)
  (10, "x", true): Int *: (String, Boolean)
  (10, "x", true): *:[Int, *:[String, *:[Boolean, EmptyTuple]]]

  inline def sayHello(name: String): Unit = println(
    s"Hello $name!"
  ) // definition-site
  @main def main() = sayHello("Maxime") // call-site

  import java.time.LocalDate
  implicit def stringToLocalDate(s: String): LocalDate = LocalDate.parse(s)

  "2018-09-01".getDayOfWeek // implicit conversion

  class IntOps(val i: Int) extends AnyVal {
    def square = i * i
  }
  implicit def intToIntOps(i: Int): IntOps = new IntOps(i)

  5.square
//The 5 gets implicitly converted to IntOps, which provides the square method.

//implicit class IntOps2(val i: Int) extends AnyVal {  def square: Int = i * i}
  constValue[9]

}
