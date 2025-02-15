import java.security.interfaces.EdECPublicKey
import java.security.spec.PKCS8EncodedKeySpec

import scala.collection.mutable.ListBuffer

class Outer {

  class Inner
  def put(inner: Inner): Unit = ()

}

//we need an instance of Outer to create an Inner instance:

val outer1 = new Outer
val inner1 = new outer1.Inner
val outer2 = new Outer
val inner2 = new outer2.Inner

//In other words, the path to an Inner instance depends upon the Outer instance. Note as well that there is no true relationship between outer1.Inner and outer2.Inner:
outer1.put(inner1) // works

//outer1.put(inner2)// does not work

//The put method on outer1 will not accept just any arbitrary instance of Inner: It must receive one derived from its own instance

case class Passenger(
  firstName: String,
  lastName: String,
  middleInitial: Option[Char]
)

class Airplane(flightNumber: Long) {

  case class Seat(row: Int, seat: Char)

  def seatPassenger(passenger: Passenger, seat: Seat): Unit = ()

}
//Given two separate Airplanes, it should be impossible to accidentally sit on the wrong plane!

val flt102 = new Airplane(102)

val flt506 = new Airplane(506)

val kelland = Passenger("Mike", "Kelland", None)

val mcadams = Passenger("Brendan", "McAdams", Some('W'))

val nash = Passenger("Michael", "Nash", None)

val flt102Seat1A = new flt102.Seat(1, 'A')

val flt506Seat21A = new flt506.Seat(21, 'A')

val flt506Seat20F = new flt506.Seat(20, 'F')

flt506.seatPassenger(nash, flt506Seat20F)    // good
flt506.seatPassenger(mcadams, flt506Seat21A) //good
flt102.seatPassenger(kelland, flt102Seat1A)  //good

//flt102.seatPassenger(kelland,flt506Seat20F)// not good

//Often, dependent method types are used in conjunction with abstract type members. Suppose we want to develop a hipsterrific key-value store. It will only support setting and getting the value for a key, but in a typesafe manner. Here is our oversimplified implementation:

object AwesomeDB {

  abstract class Key(name: String) {
    type Value
  }

}
import AwesomeDB.Key

class AwesomeDB {

  import collection.mutable.Map
  val data = Map.empty[Key, Any]

  def get(key: Key): Option[key.Value] =
    data.get(key).asInstanceOf[Option[key.Value]]

  def set(key: Key)(value: key.Value): Unit = data.update(key, value)

}

//We have defined a class Key with an abstract type member Value. The methods on AwesomeDB refer to that type without ever knowing or caring about the specific manifestation of this abstract type.

//We can now define some concrete keys that we want to use:

trait IntValued extends Key {
  type Value = Int
}

trait StringValued extends Key {
  type Value = String
}

object Keys {

  val foo = new Key("foo") with IntValued
  val bar = new Key("bar") with StringValued

}

val dataStore = new AwesomeDB
dataStore.set(Keys.foo)(23)
val i: Option[Int] = dataStore.get(Keys.foo)
//dataStore.set(Keys.foo)("23") // does not compile

///In general, whenever you want to make sure that objects created or managed by a specific instance of another type cannot accidentally or purposely be interchanged or mixed, path-dependent types are the way to go.

//Path-dependent types and dependent method types play a crucial role for attempts to encode information into types that is typically only known at runtime, for instance heterogenous lists, type-level representations of natural numbers and collections that carry their size in their type

2 :: List.empty[Int]
5 *: EmptyTuple
List(7).splitAt(0)

// val k: Int Map String = Map
//   .empty[()]
//HLists and case classes can both be used to define products of types. However HLists do not require the developer to declare class or field names. This makes them more convenient in some scenarios, for example in return types
//Right-Associative: *:[Int, *:[Int, *:[Int, *:[Int, EmptyTuple]]]] is constructed from right to left, adding elements to the tuple type from the rightmost to the leftmost.
(1, 2, 3).size
//*:  right-associative.

//In Scala, any operator ending with : (a colon) is right associative

//In simple words, right associative means evaluate expressions from right to left, and left associative means evaluate expressions from left to right

//List.empty[Int] ::7// will not work
//Here, we are getting a value :: is not a member of Int error owing to the right associative rule.

//As per the right associative rule, the Scala compiler starts evaluation of this expression from the right, as follows:
//7.::List.empty[Int]

scala.collection.mutable.ListBuffer
val li = ListBuffer(1)
li.addOne(2)
li.addOne(3)

val l = List(2)

l.prepended(1)

//the Scala compiler is trying to call the Cons :: function on 7 (which is Int). If you go through the Int class in Scala API, we don't find the Cons function for this class. That's why we are getting the value :: is not a member of Int error.
(3 *: 4 *: 5 *: EmptyTuple).head

(3 *: 4 *: 5 *: EmptyTuple).tail

3 *: 4 *: 5 *: 6 *: EmptyTuple

(3 *: 4 *: 5 *: 6 *: EmptyTuple).tail

(3 *: 4 *: 5 *: 6 *: EmptyTuple).head

(1, 2, 3) ++ (4, 5, 6)

(1, 2, 3).drop(2)

(1, 2, 3).take(2)

(1, 2, 3)(2)

(1, 2, 3, 4, 5).splitAt(2)

(1, 2, 3).zip(('a', 'b'))

(1, 'a', 2).toList

(1, 'a', 2).toArray

(1, 'a', 2).toIArray

(1, 'a').map[[X] =>> Option[X]]([T] => (t: T) => Some(t))

//Trampolining: Use trampolining techniques or functional constructs (like cats.data.Eval or scalaz.Free) to defer recursive calls and manage stack depth effectively.

enum OrderStatus {

  case Ordered
  case Processing
  case InTransit
  case Complete
  case Cancelled

}

enum PermissionsWithBits(bits: Int) {

  case READ  extends PermissionsWithBits(4) // binary 100
  case WRITE extends PermissionsWithBits(2) // binary 010
  case EXEC  extends PermissionsWithBits(1) // binary 001
  case NONE  extends PermissionsWithBits(0)

}
