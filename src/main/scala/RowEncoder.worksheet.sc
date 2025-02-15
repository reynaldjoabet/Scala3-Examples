import scala.compiletime._
import scala.deriving.Mirror
import scala.NonEmptyTuple

trait FieldEncoder[A] {
  def encodeField(a: A): String
}

type Row = List[String]

trait RowEncoder[A] {
  def encodeRow(a: A): Row
}

object BaseEncoders {

  given FieldEncoder[Int] with {
    def encodeField(x: Int) = x.toString
  }

  given FieldEncoder[Boolean] with {
    def encodeField(x: Boolean) = if x then "true" else "false"
  }

  given FieldEncoder[String] with {

    def encodeField(x: String) =
      x // Ideally, we should also escape commas and double quotes

  }

}

object TupleEncoders {

  // Base case
  given RowEncoder[EmptyTuple] with {

    def encodeRow(empty: EmptyTuple) =
      List.empty

  }

  // Inductive case
  given [H: FieldEncoder, T <: Tuple: RowEncoder]: RowEncoder[H *: T] with {

    def encodeRow(tuple: H *: T): Row =
      summon[FieldEncoder[H]].encodeField(tuple.head) :: summon[RowEncoder[T]].encodeRow(tuple.tail)

  }

}

import BaseEncoders._
import TupleEncoders._

def tupleToCsv[X <: Tuple: RowEncoder](tuple: X): List[String] =
  summon[RowEncoder[X]].encodeRow(tuple)

//tupleToCsv("Bob"*: 42*:false*:EmptyTuple)
//tupleToCsv(EmptyTuple)

//The compiler automatically generates instances of Mirror for enums and their cases, case classes and case objects, sealed classes or traits having only case classes and case objects as children.

//A match type reduces to one of its right-hand sides, depending on the type of its scrutinee

type Elem[X] = X match {
  case String      => Char
  case Array[t]    => t
  case Iterable[t] => t
  case Int         => String
}

val n: Elem[Int] = ""

val m:Elem[Array[Int]]= 9
type PolyIdentity = [T] => T => T
