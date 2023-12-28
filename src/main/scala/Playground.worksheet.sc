import scala.NonEmptyTuple
val t = (5, "String", 3d, false)
t.toList

val li = List(3, 4)
val x = t.drop(2)

t ++ x
(t ++ x).size
t.size
(1, 2, 3).size

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
      summon[FieldEncoder[H]].encodeField(tuple.head) :: summon[RowEncoder[T]]
        .encodeRow(tuple.tail)
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

enum SiteMember {
  case RegisteredUser(id: Long, email: String, isAdmin: Boolean)
  case AnonymousUser(session: String)
}

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
trait Show[A] {
  def show(a: A): String
}
object Show {
  inline def derived[T](using m: Mirror.Of[T]): Show[T] =
    inline m match {
      case s: Mirror.SumOf[T]     => ???
      case p: Mirror.ProductOf[T] => ???
    }
}
case class ISB(i: Int, s: String, b: Boolean) derives Show

enum Tree[T] derives Show {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}

Tree.Leaf(2)

//summon[Mirror.SumOf[Tree.Leaf]]

(10, "x", true): (Int, String, Boolean)
(10, "x", true): Int *: (String, Boolean)
(10, "x", true): *:[Int, *:[String, *:[Boolean, EmptyTuple]]]

class IntOps(val i: Int) {
  def square = i * i
}
implicit def intToIntOps(i: Int): IntOps = new IntOps(i)

5.square
