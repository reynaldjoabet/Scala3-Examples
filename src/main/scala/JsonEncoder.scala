import scala.deriving._
import scala.compiletime._

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
        val values     =
          a.asInstanceOf[Product].productIterator.toList

        val fields = (fieldNames zip values).map { case (fieldName, value) =>
          s""""${fieldName}": $value"""
        }
        s"{${fields.mkString(", ")}}"
      }

    }
}

object MainApp {
// Example case class
  case class Person(name: String, age: Int)

// A method that prints a JSON representation of a value
  def printJson[A](value: A)(using encoder: JsonEncoder[A]): Unit = {
    val json = encoder.encode(value)
    println(json)
  }

// Example usage
  val intJsonEncoder = summon[JsonEncoder[Int]]
  printJson(42)(using intJsonEncoder) // Output: 42

  val stringJsonEncoder = summon[JsonEncoder[String]]
  printJson("Scala")(using stringJsonEncoder) // Output: "Scala"

  val personJsonEncoder = summon[JsonEncoder[Person]]
  printJson(Person("Alice", 30))
// Output: {"name": "Alice", "age": 30}
}
