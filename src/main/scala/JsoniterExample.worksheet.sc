import model.Person

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*



final case class Driver(name: String, car: Seq[Car])

object Driver{
  given codec: JsonValueCodec[Driver] = JsonCodecMaker.make
}

final case class Car(id: Long, brand: String, model: Option[String])

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scala.util.Try
import scala.collection.immutable.Seq

trait JsoniterSyntaticSugar{
  extension (payload: String){
    def fromJson[T](using JsonValueCodec[T]): Try[T] =
      Try(readFromString(payload))
  }
  extension [T](obj: T){
    def toJson(using JsonValueCodec[T]): Try[String] =
      Try(writeToString(obj))
  }
}

object JsoniterSyntaticSugar extends JsoniterSyntaticSugar

import JsoniterSyntaticSugar.*
// serialize object to JSON string
val driver: Driver     = Driver("Paul",List.empty)
val serialized: String = writeToString(driver)

// deserialize JSON string to object
val json: String         = serialized
val deserialized: Driver = readFromString[Driver](json)

//The way I use Jsoniter is payload.fromJson , and object.toJson .


// val serialized2: String = driver.toJson

// // deserialize JSON string to object
// val json2: String         = ???
// val deserialized2: Driver = json.fromJson[Driver]

val cars: Seq[Car] =
  Seq(
    Car(1L, "Aston Martin", None),
    Car(2L, "Kenworth", Some("W900"))
  )
val driver2 = Driver("James", cars)
val json3 = driver2.toJson


import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

//implicit val personCodec: JsonValueCodec[Person] = JsonCodecMaker.make[Person](CodecMakerConfig.withCirceLikeObjectEncoding(false))

// Create a new Java Person object
  val person = new Person("John", 30)

  // Encode the Java object to JSON
  //val jsonff: String = writeToString(person)