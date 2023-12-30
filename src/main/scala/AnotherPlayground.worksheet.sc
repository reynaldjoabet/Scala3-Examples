import scala.deriving.Mirror
import scala.compiletime._
enum JsonValue {
  case JsonNull
  case JsonString(value: String)
  case JsonNumber(value: Double)
  case JsonObject(fields: Map[String, JsonValue])
  case JsonArray(elements: List[JsonValue])
}

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  // Base case: encoding for primitive types
  given JsonEncoder[Int] with {
    def encode(value: Int): JsonValue = JsonValue.JsonNumber(value.toDouble)
  }

  given JsonEncoder[String] with {
    def encode(value: String): JsonValue = JsonValue.JsonString(value)
  }
  // Derivation for arrays
  inline given derivedArray[A](using
      elemEncoder: JsonEncoder[A]
  ): JsonEncoder[Array[A]] =
    new JsonEncoder[Array[A]] {
      def encode(value: Array[A]): JsonValue =
        JsonValue.JsonArray(value.toList.map(elemEncoder.encode))
    }

  // Derivation for case classes
  inline given derived[A](using
      ev: Mirror.ProductOf[A]
  ): JsonEncoder[A] = {
    val elemInstances = summonAll[ev.MirroredElemTypes]
    new JsonEncoder[A] {
      def encode(a: A): JsonValue = {
        val fieldNames = elemLabels[ev.MirroredElemLabels]
        val values = a.asInstanceOf[Product].productIterator.toList
        val fields =
          elemInstances.zip(values).map { case (elemEncoder, elemValue) =>
            elemEncoder.asInstanceOf[JsonEncoder[Any]].encode(elemValue)
          }
        JsonValue.JsonObject(fieldNames.zip(fields).toMap)
      }
    }
  }

  inline def elemLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: elemLabels[ts]
    }
  // Helper method to summon all instances in a tuple
  inline def summonAll[T <: Tuple]: List[JsonEncoder[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[JsonEncoder[t]] :: summonAll[ts]
    }
}
case class Person(name: String, age: Int)

val person = Person("Alice", 30)

// Summon the derived instance and use it to encode the case class
val personJsonEncoder = summon[JsonEncoder[Person]]
val jsonValue: JsonValue = personJsonEncoder.encode(person)

println(jsonValue)

object Hello {}
val f: Hello.type = Hello

val n = Hello

val One = 1

//each value has its singleton type
val one: One.type = One
valueOf[23]

valueOf[Hello.type]

// Example usage
val intArray: Array[Int] = Array(1, 2, 3)
val encodedIntArray: JsonValue =
  summon[JsonEncoder[Array[Int]]].encode(intArray)
println(encodedIntArray)
