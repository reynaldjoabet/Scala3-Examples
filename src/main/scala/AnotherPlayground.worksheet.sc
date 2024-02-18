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
        val values     = a.asInstanceOf[Product].productIterator.toList
        val fields     =
          elemInstances.zip(values).map { case (elemEncoder, elemValue) =>
            elemEncoder.asInstanceOf[JsonEncoder[Any]].encode(elemValue)
          }
        JsonValue.JsonObject(fieldNames.zip(fields).toMap)
      }
    }
  }

  inline def elemLabels[T <: Tuple]: List[String]        =
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
val personJsonEncoder    = summon[JsonEncoder[Person]]
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
val intArray: Array[Int]       = Array(1, 2, 3)
val encodedIntArray: JsonValue =
  summon[JsonEncoder[Array[Int]]].encode(intArray)
println(encodedIntArray)

transparent inline def sum(a: Int, b: Int) = a + b
val total: 30                              = sum(10, 20) // The type is Literal Value 30 instead of Int

object Foo {
  val x: 3 = 3

}
summon[Foo.x.type =:= 3]

// Singletons are used to model equality between terms

def same(a: Any, b: a.type) = 9

same(3, 3)

//same(3,4)// not okay

// writing the same function with a type parameter isntead has a different meaning. It asks the compiler to find a T such that 3<:T and 4 <:T which is satisfied for T=Int

def same1[T](a: T, b: T) = 9
same1(3, 4) //okay as T is inferred as Int

//identity using dependent type
def dependedMethod(a: Any): a.type = a
val mn: 3                          = dependedMethod(3)

def guard(scope: String, a: scope.type) = 0
"challenges:read"

def middleware(permission: String, scope: permission.type) = 8

middleware("read", "read")
guard("hello", "hello")

Set(1, 2, 3, 1, 2, 3).subsetOf(Set(1, 2, 3, 8))

Set(1, 2, 3, 1, 2, 3)
