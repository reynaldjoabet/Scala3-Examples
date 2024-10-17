//Match types are a new feature in Scala 3 that let you write control structures directly at the type level:
import scala.deriving.Mirror

type Elem[X] = X match {
  case String      => Char
  case Array[t]    => t
  case Iterable[t] => t
}

case class Person(age: Int, name: String)

trait UnionOfFieldTypes[T] { type Out }

given [T](using m: Mirror.ProductOf[T]): UnionOfFieldTypes[T] with {
  type Out = Tuple.Union[m.MirroredElemTypes]
}

def acceptsAnyPersonField(using u: UnionOfFieldTypes[Person])(field: u.Out) = ???

val name = "Paul"

val html = s""" hello dude
              | how are you doing $name
              |Hope great!
""".stripMargin
