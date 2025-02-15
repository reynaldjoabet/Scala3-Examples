import scala.collection.immutable._
import scala.collection.LinearSeq
import scala.collection.SortedSet
import scala.util.CommandLineParser.FromString.given_FromString_Int
import scala.NonEmptyTuple

val t = (5, "String", 3d, false)
t.toList

val li = List(3, 4)
val x  = t.drop(2)

t.size

(3 *: "5" *: EmptyTuple).toList

t(0)

trait FieldEncoder[A] {
  def encodeField(a: A): String
}

type Row = List[String]

trait RowEncoder[A] {
  def encodeRow(a: A): Row
}

object TupleEncoders {

  // Base case
  given RowEncoder[EmptyTuple] with {

    def encodeRow(empty: EmptyTuple) =
      List.empty

  }

  // Inductive case
  given [H: FieldEncoder, T <: Tuple: RowEncoder]: RowEncoder[H *: T] with {

    def encodeRow(tuple: H *: T) =
      summon[FieldEncoder[H]].encodeField(tuple.head) :: summon[RowEncoder[T]].encodeRow(tuple.tail)

  }

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

import scala.deriving.*

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

extension (s: String) {
  def sayHello = println("thello dude")
}

2 *: "helo" *: EmptyTuple
3.3 *: true *: EmptyTuple
"".sayHello

EmptyTuple.*:(2)

trait SqlSaver[A] {
  // def save(statement: PreparedStatement, idx: Int)(a: A): Int
}

::(6, ::(3, Nil))

val vehicle = Vehicle("Hunda", 4)
vehicle.productArity

vehicle.productElement(1)

vehicle.productElementName(1)

vehicle

summon[Mirror.ProductOf[Vehicle]]

trait Random[A] {
  def generate(): A
}

3 :: Nil
Nil.::(4)

(10, "x", true): (Int, String, Boolean)
(10, "x", true): Int *: (String, Boolean)
(10, "x", true): *:[Int, *:[String, *:[Boolean, EmptyTuple]]]

class IntOps(val i: Int) {
  def square = i * i
}

implicit def intToIntOps(i: Int): IntOps = new IntOps(i)

5.square

import scala.language.strictEquality

// [2] create your class hierarchy
trait Book {

  def author: String
  def title: String
  def year: Int

}

case class PrintedBook(
  author: String,
  title: String,
  year: Int,
  pages: Int
) extends Book derives CanEqual, Show

case class AudioBook(
  author: String,
  title: String,
  year: Int,
  lengthInMinutes: Int
) extends Book derives CanEqual, Show

//Finally, use CanEqual to define which comparisons you want to allow:

// [3] create type class instances to define the allowed comparisons.
//     allow `PrintedBook == PrintedBook`
//     allow `AudioBook == AudioBook`
//given CanEqual[PrintedBook, PrintedBook] = CanEqual.derived
//given CanEqual[AudioBook, AudioBook] = CanEqual.derived

// [4a] comparing two printed books works as desired
val p1 = PrintedBook("1984", "George Orwell", 1961, 328)
val p2 = PrintedBook("1984", "George Orwell", 1961, 328)
println(p1 == p2) // true

// [4b] you can’t compare a printed book and an audiobook
val pBook = PrintedBook("1984", "George Orwell", 1961, 328)
val aBook = AudioBook("1984", "George Orwell", 2006, 682)
//println(pBook == aBook)   // compiler error
//println( aBook==pBook)
// now we can compare
// allow `PrintedBook == AudioBook`, and `AudioBook == PrintedBook`
//given CanEqual[PrintedBook, AudioBook] = CanEqual.derived
//given CanEqual[AudioBook, PrintedBook] = CanEqual.derived

import scala.compiletime._
constValue[1] == 1

type g = Vehicle.type
summon[Mirror.ProductOf[Vehicle]]

val j: 8 = 8
type myType = "Hello"
//myType is a singleton type
val b: myType = "Hello"

constValue[myType]
//"g".type
constValue[8]

pBook.asInstanceOf[Product].productIterator.toList

trait Show[A] {
  def show(a: A): String
}

object Show {

  inline def derived[T](using m: Mirror.Of[T]): Show[T] =
    new Show[T] {

      override def show(a: T): String = {
        inline m match {
          case s: Mirror.SumOf[T]     => showSum(s, a)
          case p: Mirror.ProductOf[T] => showProduct(p, a)
        }
      }

    }

  inline def elemLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: elemLabels[ts]
    }

  inline def summonAll[T <: Tuple]: List[Show[?]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Show[t]] :: summonAll[ts]

    }

  inline def showProduct[A](m: Mirror.ProductOf[A], a: A): String = {
    // m.MirroredLabel returns the singleton type eg Person.type
    // in order to get the value that has that type, we use constValue
    val productName: String =
      constValue[m.MirroredLabel] // something like "Person"
    val fieldNames = elemLabels[m.MirroredElemLabels]
    val instances: List[Show[?]] = summonAll[m.MirroredElemTypes] // eg List(Show[String],Show[Int])
    val values =
      a.asInstanceOf[Product].productIterator.toList // eg  List[Any] = List(1984, George Orwell, 1961, 328)

    val fields = fieldNames
      .zip(instances.zip(values))
      .map { case (name, (instance, value)) =>
        s"$name= ${instance.asInstanceOf[Show[Any]].show(value)}"
      }
    s"$productName(${fields.mkString(",")})"
  }

  inline def showCase[A, T <: Tuple](n: Int, ord: Int, a: A): String = {
    inline erasedValue[T] match {
      case _: EmptyTuple => ""
      case _: (t *: ts) =>
        if (n == ord) {
          summonFrom { case p: Mirror.ProductOf[`t`] =>
            showProduct(p, a.asInstanceOf[t])
          }
        } else showCase[A, ts](n + 1, ord, a)
    }
  }

  inline def showSum[A](m: Mirror.SumOf[A], a: A): String = {
    val ord = m.ordinal(a)
    showCase[A, m.MirroredElemTypes](0, ord, a)
  }

  given Show[Int] = new Show[Int] {
    override def show(a: Int): String = a.toString()
  }

  given Show[String] = new Show[String] {
    override def show(a: String): String = a
  }

  given Show[Boolean] = new Show[Boolean] {
    override def show(a: Boolean): String = a.toString
  }

  given Show[Double] = new Show[Double] {
    override def show(a: Double): String = a.toString
  }

  given Show[Float] = new Show[Float] {
    override def show(a: Float): String = a.toString
  }

  given Show[Long] with {
    override def show(a: Long): String = a.toString()
  }

  inline given derivedArray[A](using elemShow: Show[A]): Show[Array[A]] =
    new Show[Array[A]] {

      def show(value: Array[A]): String =
        s"List(${value.map(elemShow.show).mkString(",")})"
      // s"List(${})"

    }

  inline given derivedLinearSeq[A](using
    elemShow: Show[A]
  ): Show[LinearSeq[A]] =
    new Show[LinearSeq[A]] {

      def show(value: LinearSeq[A]): String =
        value.map(elemShow.show).mkString(",")

    }

  inline given derivedOption[A](using elemShow: Show[A]): Show[Option[A]] =
    new Show[Option[A]] {

      def show(value: Option[A]): String =
        if (value.isDefined) s"Some(${value.map(elemShow.show).mkString(",")})"
        else None.toString()

    }

  inline given derivedSortedSet[A](using
    elemShow: Show[A]
  ): Show[SortedSet[A]] =
    new Show[SortedSet[A]] {

      def show(value: SortedSet[A]): String =
        s"List(${value.map(elemShow.show).mkString(",")})"

    }
    inline given derivedSet[A](using elemShow: Show[A]): Show[Set[A]] =
      new Show[Set[A]] {

        def show(value: Set[A]): String =
          s"Set(${value.map(elemShow.show).mkString(",")})"

      }

    inline given derivedTreeSet[A](using elemShow: Show[A]): Show[TreeSet[A]] =
      new Show[TreeSet[A]] {

        def show(value: TreeSet[A]): String =
          s"TreeSet(${value.map(elemShow.show).mkString(",")})"

      }

  inline given derivedMap[A, B](using
    elemShow: Show[A],
    keyShow: Show[B]
  ): Show[Map[A, B]] =
    new Show[Map[A, B]] {
      def show(value: Map[A, B]): String = ""
    }

  inline given derivedList[A](using elemShow: Show[A]): Show[List[A]] =
    new Show[List[A]] {

      def show(value: List[A]): String =
        s"List(${value.map(elemShow.show).mkString(",")})"

    }

  inline given derivedVector[A](using elemShow: Show[A]): Show[Vector[A]] =
    new Show[Vector[A]] {

      def show(value: Vector[A]): String =
        s"Vector(${value.map(elemShow.show).mkString(",")})"

    }

  inline given derivedListSet[A](using elemShow: Show[A]): Show[ListSet[A]] =
    new Show[ListSet[A]] {

      def show(value: ListSet[A]): String =
        value.map(elemShow.show).mkString(",")

    }

  val n = valueOf[12]

}

case class ISB(i: Double, s: String, b: Boolean, p: PrintedBook) derives Show, JsonEncoder

val isb = ISB(45.7, "Java and Scala", true, p = p2)

summon[Show[ISB]].show(isb)

summon[Show[PrintedBook]].show(p1)
//summon[Show[AudioBook]].show(aBook)("hello", "dude").toList.mkString(",")

summon[Show[List[Int]]].show(List(2, 3, 44, 5, 4))

summon[Show[List[PrintedBook]]].show(List(p1, p2))

summon[Show[Set[PrintedBook]]].show(Set(p1, p2))

summon[Show[SortedSet[Int]]].show(SortedSet(1, 2, 3, 4))

summon[Show[Option[Int]]].show(Some(23))
summon[Show[Option[Int]]].show(None)

enum SiteMember derives Show {

  case RegisteredUser(id: Long, email: String, isAdmin: Boolean)
  case AnonymousUser(session: String)

}

summon[Show[SiteMember]].show(SiteMember.AnonymousUser("newSession"))

// Mirror for the SiteMemeber enum
new Mirror.Sum {

  type MirroredElemTypes = (SiteMember.RegisteredUser, SiteMember.AnonymousUser)
  override def ordinal(x: MirroredMonoType): Int = 2

}

// Mirror for the RegisteredUser case class
new Mirror.Product {

  type MirroredElemTypes = (Long, String, Boolean)
  def fromProduct(p: Product): MirroredMonoType = ???
  // new RegisteredUser(...)

}

summon[Mirror.SumOf[SiteMember]].ordinal(SiteMember.AnonymousUser(""))

constValue["hl"]

constValue["element"]
List(7).::(8).appended(0).prepended(45)

object A { val x = 42 }
implicitly[A.type <:< Singleton]

implicitly[A.x.type <:< Singleton]

implicitly[42 <:< Singleton]

enum Day derives Show {
  case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
}

summon[Mirror.SumOf[Day]].ordinal(Day.Monday)
summon[Mirror.SumOf[Day]].ordinal(Day.Tuesday)
summon[Mirror.SumOf[Day]].ordinal(Day.Wednesday)
summon[Mirror.SumOf[Day]].ordinal(Day.Thursday)
summon[Mirror.SumOf[Day]].ordinal(Day.Friday)
summon[Mirror.SumOf[Day]].ordinal(Day.Saturday)
summon[Mirror.SumOf[Day]].ordinal(Day.Sunday)

summon[Show[Day]].show(Day.Friday)

trait JsonEncoder[A] {
  def encode(value: A): String
}

object JsonEncoder {

  given JsonEncoder[Int] with {
    def encode(value: Int): String = s"$value"
  }

  given JsonEncoder[String] with {
    def encode(value: String): String = s""""$value""""
  }

  inline def elemLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: elemLabels[ts]
    }

  // Generic derivation using `erasedValue` for product types (case classes)
  inline given derived[A](using m: Mirror.Of[A]): JsonEncoder[A] =
    new JsonEncoder[A] {

      def encode(a: A): String = {
        val fieldNames = elemLabels[m.MirroredElemLabels]
        val values =
          a.asInstanceOf[Product].productIterator.toList

        val fields = fieldNames
          .zip(values)
          .map { case (fieldName, value) =>
            s""""$fieldName": ${if (value.isInstanceOf[AnyRef]) value else value}"""
          }
        s"{${fields.mkString(", ")}}"
      }

    }

}

// Example case class
case class Person(name: String, age: Int)

// A method that prints a JSON representation of a value
def printJson[A](value: A)(using encoder: JsonEncoder[A]): Unit = {
  val json = encoder.encode(value)
  println(json)
}

// Example usage
val intJsonEncoder = summon[JsonEncoder[Int]]
printJson(42) // Output: 42

val stringJsonEncoder = summon[JsonEncoder[String]]
printJson("Scala") // Output: "Scala"

val personJsonEncoder = summon[JsonEncoder[Person]]
printJson(Person("Alice", 30))
// Output: {"name": "Alice", "age": 30}

printJson(isb)

summon[Mirror.ProductOf[ISB]]

{}
