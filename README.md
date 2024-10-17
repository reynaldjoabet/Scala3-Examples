# Scala3-Examples

When we need to use an implicit instance we use using keyword. So, now the summoner method will look like:

```scala
object SqlSaver {
  def apply[T](using ss: SqlSaver[T]): SqlSaver[T] = ss
}
```
When we need to declare an instance of type class (or any other implicit), we use given keyword. With this new syntax instances of SqlSaver for primitive types become:
```scala
object SqlSaver {
  // ...
  given SqlSaver[Int] = createSimpleSaver((a, s, i) => s.setInt(i, a))
  given SqlSaver[String] = createSimpleSaver((a, s, i) => s.setString(i, a))
  given SqlSaver[Double] = createSimpleSaver((a, s, i) => s.setDouble(i, a))
  given SqlSaver[BigDecimal] = createSimpleSaver((a, s, i) => s.setBigDecimal(i, a.underlying))
  given SqlSaver[LocalDateTime] = 
    createSimpleSaver((a, s, i) => s.setTimestamp(i, Timestamp.valueOf(a)))
}
```

In this example we created anonymous instances, but it’s also possible to give names to the type class instances, e.g.:


```scala
given intSaver: SqlSaver[Int] = createSimpleSaver((a, s, i) => s.setString(i, a))
```

`3.3*:true*:EmptyTuple`
`*:[Double,*:[Boolean,EmptyTuple]]` from `sealed abstract class *:[+H, +T <: Tuple] extends NonEmptyTuple`


`::(6,::(3,Nil))` will produce List(6,3)

```scala
final case class Vehicle(manufacturer: String, wheels: Int)
summon[Mirror.ProductOf[Vehicle]]
```
we can create instance for Products, which will convert a case class to tuple
summoning the implicit or given instance for this case class will give us `Product` with the following
```scala
scala.deriving.Mirror.Product{type MirroredMonoType = Vehicle; type MirroredType = Vehicle; type MirroredLabel = "Vehicle"; type MirroredElemTypes = (String, Int); type MirroredElemLabels = ("manufacturer", "wheels")}
```

For example, if the product is `case class Foo(a: String, b: Int)` then the tuple type will be `(String, Int)`, or that’s the same `String *: Int *: EmptyTuple`. 


In shapeless for Scala 2, we used Generic to convert ADTs to and from HLists. It also was a type link between ADTs and their HList representations. In Scala 3, we have class Mirror to connect products and coproducts with tuples both on the type and the value level.

Mirrors, tuples, or inlining, to derive instances 

### The Mirror typeclass
The Mirror typeclass gives us information about the type itself, the names of the fields, the types of the fields, and how it all fits together. The compiler is able to provide this information for:
- Case classes
- Case objects
- Sealed hierarchies containing case object and case classes
- Enums
- Enum cases

```scala
enum SiteMember{
  case RegisteredUser(id: Long, email: String, isAdmin: Boolean)
  case AnonymousUser(session: String)
}
```
 type class derivation, is a method of creating an implementation of a type class with the aid of the compiler

 These more complex data types are product types (case classes containing multiple fields), sum types (sealed traits with well known descendants), and combinations of them

 To summarize, we need three capabilities to implement our random generation algorithm:

- Inspect Enum's and get their subtypes:
- Inspect case classes and get their field types:
- Instantiate a case class with a list of its parameter values:

### Using Mirror to inspect ADTs
All of these capabilities can be implemented using a type level mechanism called `Mirror`. Mirror is a type class in the Scala 3 standard library that provides information about ADTs. We can ask the compiler for a mirror by adding an implicit parameter. Here’s the mirror for the `SiteMember` enum and for the `RegisteredUser` case class(some fields were omitted):

```scala
// Mirror for the SiteMemeber enum
new Mirror.Sum:
  type MirroredElemTypes = (RegisteredUser, AnonymousUser)
  // ...
// Mirror for the RegisteredUser case class
new Mirror.Product:
  type MirroredElemTypes = (Long, String, Boolean)
  def fromProduct(p: Product): MirroredMonoType =
    new RegisteredUser(...)
  // ...
```

The mirror field we’ll need is MirroredElemTypes, which is a type level tuple containing types. Those types are the types of a case class parameters or the subtypes of an enum. This answers our first two requirements. The fromProduct method, which enables us to instantiate a case class from a tuple of constructor parameter values, answers the third.

A mirror is generated for every case class, sealed trait, and enum in your code, so it needs to be low footprint. That’s one of the reasons it’s implemented only as a compile time mechanism. Note that MirroredElemTypes is not a val or a def, it’s a type. That presents a difficulty — how can we work with types that exist only at compile time, as opposed to (runtime) values?

### Running code at compile time using inline

`inline` is a powerful new keyword that can be used in several ways — the main thing inline enables is to perform computations at compile time.
`summon` is the Scala 3 equivalent to `implicitly`


To create a generic type class derivation natively in Scala 3, the steps are pretty similar to shapeless:

- Find generic representation of type A, that is, break A into Product and Coproduct combinations
- Implement instances for Product and Coproduct types.

In our example, to derive the generic representation of Tree, we can simply define a derived function:

```scala
inline def derived[T](using m: Mirror.Of[T]): Show[T] =
  inline m match
    case s: Mirror.SumOf[T] => ???
    case p: Mirror.ProductOf[T] => ???
```
By using m: Mirror.Of[T], the compiler will automatically derive the Mirror.Of[T] type from T, which will look like:


the derived method takes a context parameter of (some subtype of) type Mirror which defines the shape of the deriving type T, and computes the type class implementation according to that shape.
Hence, the process to properly show Tree:

compiler derives Mirror.Of[Tree], the result is a Mirror.Sum
break into Mirror.Sum, if type is Mirror.ProductOf[Branch], recursively show its left and right
if type is Mirror.ProductOf[Leaf], recursively show its elem

Product types (i.e. case classes and objects, and enum cases) have mirrors which are subtypes of Mirror.Product. Sum types (i.e. sealed class or traits with product children, and enums) have mirrors which are subtypes of Mirror.Sum

Note that `derived` is defined as an inline def. This means that the method will be inlined at all call sites (for instance the compiler generated instance definitions in the companion objects of ADTs which have a deriving Eq clause).

## Uses of metaprogramming
- Generating code to avoid boilerplate
- Analzing code to detect errors
- Transforming code eg into more performant code
- Running compuation at compile-time instead of at run-time

the `inline` modifier moves code from definition-site to call-site
```scala
inline def sayHello(name:String):Unit=println(s"Hello $name!")// definition-site
@main def main()= sayHello("Maxime")// call-site
```
This is compiled to:
```scala
@main def main()= println(s"Hello ${"Maxime"}!")
```
## Uses of inline defs
- useful for performance as we avoid the overhead of a function call
- Unlocks optimzation: inlining allows the compiler to do powerful optimizations

`inline val` moves values from definition-site to use-site
`inline if` ensures if is simplified to one of its branches
`inline match` ensures pattern matching is simplified to its cases

```scala
(10,"x",true):(Int,String,Boolean)
(10,"x",true):Int *:(String,Boolean)
(10,"x",true): *:[Int,*:[String,*:[Boolean,EmptyTuple]]]
```


## Implicit Conversion
```scala
import java.time.LocalDate
  implicit def stringToLocalDate(s: String): LocalDate = LocalDate.parse(s)

  "2018-09-01".getDayOfWeek // implicit conversion

  class IntOps(val i: Int) extends AnyVal {
    def square = i * i
  }
  implicit def intToIntOps(i: Int): IntOps = new IntOps(i)

  5.square
//The 5 gets implicitly converted to IntOps, which provides the square method.

implicit class IntOps2(val i: Int) extends AnyVal {  def square: Int = i * i}
```
The instances package contains the implementations of the type classes for basic data types that are present in the language core and the ones defined by the Cats library.

```scala
final class MonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def whileM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.whileM(p)(fa)
  def whileM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.whileM_(p)(fa)
  def untilM[G[_]](p: F[Boolean])(implicit M: Monad[F], G: Alternative[G]): F[G[A]] = M.untilM(fa)(p)
  def untilM_(p: F[Boolean])(implicit M: Monad[F]): F[Unit] = M.untilM_(fa)(p)
  def iterateWhile(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateWhile(fa)(p)
  def iterateUntil(p: A => Boolean)(implicit M: Monad[F]): F[A] = M.iterateUntil(fa)(p)
}
trait MonadSyntax {
  implicit final def catsSyntaxMonad[F[_], A](fa: F[A]): MonadOps[F, A] = new MonadOps(fa)
}
```
`erasedValue[T]`  and `constValue[]`



In Scala, any operator ending with `:` (a colon) is right associative. Right associative means evaluate expressions from right to left, and left associative means evaluate expressions from left to right.
Methods that end with the “:” character are invoked by passing left operand to the one in right
```scala
val myList = 1 :: 2 :: 3 :: Nil
```


Mirror type class instances provide information at the type level about the components and labelling of the type. They also provide minimal term level infrastructure to allow higher level libraries to provide comprehensive derivation support.
`Product types` (i.e. case classes and objects, and enum cases) have mirrors which are subtypes of `Mirror.Product. Sum` types (i.e. sealed class or traits with product children, and enums) have mirrors which are subtypes of `Mirror.Sum`.

A trait or class can appear in a `derives` clause if its companion object defines a method named `derived`. The signature and implementation of a `derived` method for a type class `TC[_]` are arbitrary but it is typically of the following form

```scala
def derived[T](using Mirror.Of[T]): TC[T] = ...
```

The `derived` method takes a context parameter of (some subtype of) type `Mirror` which defines the shape of the deriving type `T`, and computes the type class implementation according to that shape. This is all that the provider of an ADT with a `derives` clause has to know about the derivation of a type class instance.

`circe` is a type class derivation library, allowing users to derive type classes for their data types. It does this in two ways, automatic and semiautomatic

A type class with multiple methods is defined as 
```scala
trait HasLegs[A]{
    extension(a:A){
        def walk():Unit
        def run():Unit
    }
}
```
Universal equality or loose equality lets us compare any two variables to each other, even if they’re of different types
universal equality vs multiversal equality.This may lead to several unintended problems in our programs.

```scala
case class Square(length: Float)
case class Circle(radius: Float)
val square = Square(5)
val circle = Circle(5)

println(square == circle) // prints false. No compilation errors
```

The program compiles successfully without any problems, but it starts causing trouble at runtime

in Scala3, we can disable universal equality  by importing `scala.language.strictEquality` or by adding `language.strictEquality` as a compiler flag

multiversal equality to address the problems caused by universal equality.
multiversal equality uses the binary type class `CanEqual` to indicate that values of two given types can be compared with each other


`erasedValue,summonInline and ConstValue`

The `inline` keyword is an instruction for the compiler to copy the code from the definition site and to compile it at the caller site

To handle non-literal types, the `erasedValue` method in the same package often comes in handy. It’s undoubtedly one of the most dazzling tools that helps unleash the power of literal types.

Inlining offers many advantages, one of which is to help the compiler resolve a type parameter to a concrete type provided by the caller. The inlined method can then pass the resolved T to `erasedValue[T]`


There are two ways to summon values in `inline` methods, the first is with a using parameter and the second is with one of `summonInline`, `summonAll` or `summonFrom`. `using` will summon the value at call site before inlining as if the method was not inline. On the other hand, `summonInline` will summon after inlining if the call is not eliminated form a dead branch. `SummonAll` provides a way to summon multiple values at the same time from a tuple type. `summonFrom` provides a way to try several implicit searches.

summonInline: enables us to summon typeclass instances at a whim for any abstract parameter without specifying it in the method definition, it delays the evaluation of summoning until the call is fully inlined,

```scala
def tired[A](using Ordering[A]): Ordering[A] =
  summon[Ordering[A]]

inline def wired[A]: Ordering[A] =
  summonInline[Ordering[A]]
```

erasedValue: this one is especially handy when dealing with Tuples of literal types, which allows us to pretty much treat them as compile time collections. What’s weird about this method is it forces us to pattern match on its types instead of values, 



`HList` is a combination of characteristics of both lists and tuples:

Tuples are fixed lengths of elements of distinct types at compile time. Once we fix the arity of tuples, we will be stuck with them. On the other hand, each element of a tuple can be of different types but after definition, we can’t extend the arity of a tuple.
Lists are variable-length sequences of elements all of the same type.
`HList` has combined features of tuples and lists. It captures the sequence of distinct types from tuples and captures the variable-length sequence of elements from lists. So, HList is a variable-length sequence of elements of distinct types:


Scala allows types to be expressed using infix notation when they consist of two type parameters.
Instead of writing `A[B]`, you can write `A B`, which is especially common when dealing with type constructors like `List[Int]` can be written as `List Int`.
Right-Associative Type Alias:

The :: symbol is right-associative when used in type declarations.
This means that A :: B :: C is interpreted as A :: (B :: C), making it convenient for constructing type-level lists or sequences.
Combining these features, the type H :: HList is interpreted as ::[H, HList]. The :: case class or trait is often defined with two type parameters, allowing it to be used in this infix notation for constructing type-level lists or HLists.

Methods ending in ‘:’ are right-associative, while others are left-associative.

```scala
sealed trait HList extends Product with Serializable{
    def prepend[H](h : H): ::[H,HList]= new ::(h, this)
    def ::[H](h : H): H::HList= new ::(h, this)
  }

  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList{

  }

  sealed trait HNil extends HList {
    
  }

  case object HNil extends HNil

  val h=5::HNil
val hlist = 1 :: 1.0 :: "One" :: false ::‌ HNil
```
HList is a recursive data structure. Each HList is either an empty list (HNil) or a pair of H and T types, in which H (head) is an arbitrary type and T (tail) is another HList. In Scala, we can write any pair type like ::[H, T] in a more ergonomic way like H :: T, so the type of our hlist is either Int :: Double :: String :: Boolean :: HNill or ::[Int, ::[Double, ::[String, ::[Boolean, HNill]]]].

`Singleton` is used by the compiler as a supertype for singleton types. This includes literal types, as they are also singleton types.

A singleton type is a type inhabited by exactly one value
Scala 2.13 introduced a special kind of singleton type—the literal type. It denotes a single value of some literal and represents the most precise type of this literal

`object Foo{}` The type `Foo.type` is the type of `Foo`, and Foo is the only value with that type.
`constValue`  allows you to extract the value of a constant at compile-time; the ability to obtain the value of a constant, such as a literal or an enum case, during the compilation process rather than at runtime

`refined` uses singleton types
Refined types allow you to define more specific types by applying predicates to existing types
```scala
import scala.compiletime.ops.int._
```

A `Refined p x` wraps a value of type `x`, ensuring that it satisfies a type-level predicate `p`.
`slice :: Refined Positive Int -> [a] -> [[a]]`

In Scala we have it as
`Int Refined Positive` or `val aPositiveInteger: Refined[Int, Positive] = 42`
Refined is a type that takes two type arguments: the “value” type you’re testing/validating, and the predicate you want the value to pass
```scala
val aNegative: Int Refined Negative = -100
val nonNegative: Int Refined NonNegative = 0
val anOdd: Int Refined Odd = 3
val anEven: Int Refined Even = 68
```
Notice I used Refined in infix notation: Refined[Int, Odd] can also be written as Int Refined Odd

We can instruct the compiler to use the most specialized type available, by using the keyword `transparent`:

inline val instructs the compiler that the value is a compile-time constant and that the variable can be replaced with the actual value at compile time

inline def moves the code from definition-site to call-site.

instances of `=:=` type are generated automatically by the compiler when the left-hand side and the right-hand side are both subtypes of each other
`summon[X=:=Y]` compiles only if X are equivalent Y
```scala
object Foo{
    val x:3=3

}
summon[Foo.x.type =:=3]
```
Match types are well suited when we want to implement methods whose implementation depends on the type(s) of its parameters. Such methods are called dependent methods

parametric polymorphism is when the behavior of your program does not change depending on the shape of your types

```scala
List("Paul","John").size
List(1,2,3).size
```
In adhoc polymorphism, the behavior of your program changes depending on the shape of your types
In OOP, this achieved via subtyping
Functional progamming languages like Haskell do not have subtyping but they still need adhoc polymorphism,so they use type classes 


A match type reduces to one of its right-hand sides, depending on the type of its scrutinee
```scala
type Elem[X] = X match
  case String => Char
  case Array[t] => t
  case Iterable[t] => t

```

Sahpeless was used for type class derivation and arity and data polymorphsim


You can think of type constructors as regular constructors that operate at the type level. Instead of taking other values as arguments and yielding new values in return, type constructors take types and yield types

## Free Monad
Free as in free to be interpreted in any way. free is used in the sense of unrestricted rather than zero cost
All of the doobie monads are implemented via Free and have no operational semantics; we can only “run” a doobie program by transforming FooIO (for some carrier type java.sql.Foo) to a monad that actually has some meaning.

Out of the box doobie provides an interpreter from its free monads to Kleisli[M, Foo, ?] given Async[M]

```scala
val interpreter = KleisliInterpreter[IO](LogHandler.noop).ConnectionInterpreter
// interpreter: ConnectionOp ~> [γ$9$]Kleisli[IO, Connection, γ$9$] = doobie.free.KleisliInterpreter$$anon$10@19a97a54
val kleisli = program1.foldMap(interpreter)
```

So the interpreter above is used to transform a ConnectionIO[A] program into a Kleisli[IO, Connection, A]. Then we construct an IO[Connection] (returning null) and bind it through the Kleisli, yielding our IO[Int]. This of course only works because program1 is a pure value that does not look at the connection.

The Transactor that we defined at the beginning of this chapter is basically a utility that allows us to do the same as above using program1.transact(xa).

There is a bit more going on when calling transact (we add commit/rollback handling and ensure that the connection is closed in all cases) but fundamentally it’s just a natural transformation and a bind.

```scala
 // Free monad over ConnectionOp.
  type ConnectionIO[A] = FF[ConnectionOp, A]
```
Algebra and free monad for primitive operations over a java.sql.Connection. This is a low-level API that exposes lifecycle-managed JDBC objects directly and is intended mainly for library developers. End users will prefer a safer, higher-level API such as that provided in the doobie.hi package.

ConnectionIO is a free monad that must be run via an interpreter, most commonly via natural transformation of its underlying algebra ConnectionOp to another monad via Free#foldMap.

The library provides a natural transformation to Kleisli[M, Connection, A] for any exception-trapping (Catchable) and effect-capturing (Capture) monad M. Such evidence is provided for Task, IO, and stdlib Future; and transK[M] is provided as syntax.


For functors, the higher-kinded version of Function1 is called a natural transformation. The structure looks something like this:

```scala
trait FunctorNatTrans[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
}
```

Examples of natural transformations can be found throughout the Scala standard library, if you know how to look for them. The .headOption method on lists is a good example of a natural transformation from lists to options:

```scala
object ListToOptionTrans extends FunctorNatTrans[List, Option] {
    override def apply[A](fa: List[A]) = fa.headOption
}
```

Doobie is a free monad

```scala

 FunctionK[F[_], G[_]] is a functor transformation from `F` to `G`
in the same manner that function `A => B` is a morphism from values
 of type `A` to `B`. An easy way to create a FunctionK instance is to use the Polymorphic lambdas provided by typelevel/kind-projector v0.9+. E.g.
  {{{
    val listToOption = λ[FunctionK[List, Option]](_.headOption)
  }}}
 
trait FunctionK[F[_], G[_]] extends Serializable { self =>

  /**
   * Applies this functor transformation from `F` to `G`
   */
  def apply[A](fa: F[A]): G[A]

}
//Catamorphism for `Free`
 final def foldMap[M[_]](f: FunctionK[S, M])(implicit M: Monad[M]): M[A] =
    M.tailRecM(this)(_.step match {
      case Pure(a)          => M.pure(Right(a))
      case Suspend(sa)      => M.map(f(sa))(Right(_))
      case FlatMapped(c, g) => M.map(c.foldMap(f))(cc => Left(g(cc)))
    })

```

A free monad is a construction which allows you to build a monad from any Functor. Like other monads, it is a pure way to represent and manipulate computations.
In particular, free monads provide a practical way to:
- represent stateful computations as data, and run them
- run recursive computations in a stack-safe way
- build an embedded DSL (domain-specific language)
- retarget a computation to another interpreter using natural transformations

 free monads are basically a way to easily get a generic pure Monad instance for any Functor


The concept of “natural transformation” is a higher-kinded Function1 type that looks like this:
```scala
 trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }
```
So instead of a regular Function1 taking value types as type parameters, now we operate at a higher kind
Examples of natural transformations in real life include:

`Try[A].toOption`: this is an example of an implementation of a natural transformation between Try and Option
`List[A].headOption` which returns the head of the list, if it exists: an example of an implementation of a natural transformation between List and Option
`Option[A].toList`: the reverse


The Free monad is a pattern which allows us to separate

- the description of fundamental operations
- the business logic of our application
- the evaluation of that business logic
 
This makes it very easy to maintain, because we can work independently on either

- the business logic, while keeping interpreters fixed
- the interpreters, while keeping the business logic fixed
- the fundamental operations and the interpreter(s), while keeping the business logic fixed
So notice the flexibility we get by choosing to work on a piece of the system without affecting the others. Another benefit of this approach is testability, because we can always supply a “testing” monad to evaluate the program, make assertions and ensure the business logic is correct, while the interpreter itself can be independently tested.

The Free Monad is a design pattern used to create a domain-specific language (DSL) in a functional programming language. It allows you to represent a sequence of operations as a data structure, enabling the separation of the description of the computation from its interpretation.

```scala
val interpreter: MyDSL ~> Id = new (MyDSL ~> Id) {
  def apply[A](fa: MyDSL[A]): Id[A] = fa match {
    case Operation1(data) => println(s"Operation1: $data")
    case Operation2(value) => value * 2
  }
}

val result: Int = myProgram.foldMap(interpreter)
```
`foldMap` is used to interpret the Free Monad, executing the program with the specified interpreter. The interpreter transforms each DSL operation into its corresponding effect.

Free Monads are particularly useful in functional programming for creating modular and composable DSLs. They allow you to separate the definition of operations from their interpretation, making your code more maintainable and extensible.

## Error Handling:
Free Monads can handle errors gracefully. You might use an Either or Option in your DSL operations to represent success or failure:

```scala
sealed trait MyDSL[A]
case class Operation1(data: String) extends MyDSL[Either[String, Unit]]
case class Operation2(value: Int) extends MyDSL[Either[String, Int]]

```

## Final Tagless Encoding:
Another approach to building DSLs in Scala is the "final tagless" encoding. It involves using type classes to represent operations, providing flexibility and better type inference.

```scala
trait FileDSL[F[_]] {
  def readFile(path: String): F[String]
  def writeFile(path: String, content: String): F[Unit]
}

def program[F[_]: FileDSL]: F[String] = {
  import cats.syntax.all._

  for {
    content <- FileDSL[F].readFile("local/path/to/file.txt")
    _       <- FileDSL[F].writeFile("cloud/path/to/file.txt", content)
  } yield content
}

```
This approach eliminates the need for a Free Monad but retains many benefits of separation between program description and interpretation. 

In functional programming, the term "free monad" refers to a particular construction that allows you to build a monad for a given functor without imposing any additional constraints on that functor. A "free monad" is essentially a way to lift a functor into a monad.
`Pull` is tail-recursive, which is an essential characteristic of the free monad. This enables the chaining of computations without causing a stack overflow.

The "free" part refers to the idea that you can create a data structure that represents a sequence of computations (like a DSL - Domain Specific Language) without actually executing them. This structure is then interpreted later.

The Tagless Final encoding is an alternative to Free Monads that achieves similar separation of concerns but with improved type safety. Instead of using an algebraic data type (ADT) to represent operations, Tagless Final uses type classes.
##  type class compose

type class composition involves combining multiple type classes to create a new type class or set of behaviors. This can be achieved through the use of implicits and implicit parameters

```scala
trait Show[A] {
  def show(value: A): String
}

trait Printable[A] {
  def print(value: A): String
}

implicit def printableShow[A](implicit showInstance: Show[A]): Printable[A] = new Printable[A] {
  def print(value: A): String = showInstance.show(value)
}

```
Tagless final encoding is an approach in functional programming that uses type classes to represent algebras. This technique is particularly powerful when defining domain-specific languages (DSLs) or working with abstract syntax trees (ASTs). The encoding helps separate the description of operations from their interpretation.

```scala
def genericFunction[F[_]: Functor, A](fa: F[A]): F[A] =
  Functor[F].map(fa)(identity)


```
Doobie is a pure functional JDBC layer for Scala. It uses type classes to define how data types are mapped to and from SQL types.

```scala
import doobie._
import doobie.implicits._

case class Person(id: Long, name: String, age: Int)

implicit val personMeta: Meta[Person] =
  Meta[(Long, String, Int)].timap { case (id, name, age) => Person(id, name, age) }(person => (person.id, person.name, person.age))

val query: Query0[Person] = sql"SELECT id, name, age FROM persons".query[Person]

```

`Meta` is a type class that defines how to map between a SQL result set and the Scala type

Combining type classes with tagless final encoding or free monads allows you to define expressive DSLs for building complex programs with clear separation of concerns

```scala
trait UserRepository[F[_]] {
  def getUser(id: Long): F[Option[User]]
  def saveUser(user: User): F[Unit]
}

def program[F[_]: Monad](repo: UserRepository[F]): F[Unit] =
  for {
    user <- repo.getUser(123)
    _    <- user.traverse(u => repo.saveUser(u.copy(name = "Updated")))
  } yield ()

```
`UserRepository` is a type class, and the program is written generically over any monad `F`


```scala
class ConnectionIOOps[A](ma: ConnectionIO[A]) {
  def transact[M[_]: MonadCancelThrow](xa: Transactor[M]): M[A] = xa.trans.apply(ma)
}
 /**
     * Natural transformation that provides a connection and binds through a `ConnectionIO` program
     * interpreted via the given interpreter, using the given `Strategy`, yielding an independent
     * program in `M`. This is the most common way to run a doobie program.
     * @group Natural Transformations
     */
    def trans(implicit ev: MonadCancelThrow[M]): ConnectionIO ~> M =
      new (ConnectionIO ~> M) {
        def apply[T](f: ConnectionIO[T]): M[T] =
          connect(kernel).use { conn =>
            strategy.resource.use(_ => f).foldMap(interpret).run(conn)
          }
      }
```
A natural transformation is a concept from category theory and functional programming that describes a conversion between two type constructors in a way that respects the structure of the types.


In Doobie, the interpreter is a `Transactor` and the method used to execute `trans`

In Scala, you can define infix types using symbolic names, and `~>` is an example of such a symbolic name. It's a convenient shorthand for writing `~>[...]`. When you see `ConnectionIO ~> M`, it means the same thing as `~>[ConnectionIO, M]`



In ad-hoc polymorphism, the behavior of a function or operator changes based on the types.

Type classes provide a way to achieve ad-hoc polymorphism by allowing different types to exhibit a common behavior defined by the type class.
Instances of type classes are provided for specific types, allowing the same polymorphic function to work with various types that adhere to the type class interface.


Ad-hoc Polymorphism with Type Classes:

Type classes provide a way to achieve ad-hoc polymorphism by allowing different types to exhibit a common behavior defined by the type class.
Instances of type classes are provided for specific types, allowing the same polymorphic function to work with various types that adhere to the type class interface.
Separation of Concerns:

Type classes help in separating concerns and achieving a clean separation between interfaces (type classes) and instances (implementations for specific types).
Polymorphic functions can be defined independently of specific types, promoting modularity and code reuse.

Type classes enable ad-hoc polymorphism by allowing different types to have different implementations for the same functions.


given a function f: B => A, if you have a function g: C => B, you can use contramap to obtain a new function h: C => A by "pre-applying" g to the input of f.

## Refined
refined types are a way to define more specific types by adding constraints or refining existing types. These constraints are expressed as predicates that the values of the refined type must satisfy. Refined types are often used for creating more precise domain-specific types.

The primary features in Scala that power refined types come from the combination of type-level programming features, literal types (introduced in Scala 2.13), and the ability to define and manipulate singleton types. 

## Opaque types
```scala
object types {
  opaque type Year = Int
}
```
We’ve now created a new opaque type that is equivalent to an Int within the scope where it is defined. But outside the scope where it is defined, Year and Int are not the same
[type class derivation](https://medium.com/riskified-technology/type-class-derivation-in-scala-3-ba3c7c41d3ef)
[Scala3](http://www.limansky.me/posts/2021-07-26-from-scala-2-shapeless-to-scala-3.html)

[metaprogramming](https://scalac.io/blog/inline-your-boilerplate-harnessing-scala3-metaprogramming-without-macros/)

[literal types](https://medium.com/@hao.qin/scala-3-enlightenment-unleash-the-power-of-literal-types-41e3436b4df8#:~:text=Literal%20types%20are%20compilation%2Dtime,available%20to%20any%20Scala%20programmer.)

[inline](https://scalac.io/blog/inline-your-boilerplate-harnessing-scala3-metaprogramming-without-macros/)

[inline](https://www.baeldung.com/scala/inline-modifier)
[generic-type-class-derivation](https://blog.1punch.dev/scala/dotty/en/generic-type-class-derivation)

[automatic-type-class-derivation](https://medium.com/@mattroberts297/automatic-type-class-derivation-with-shapeless-part-three-357709122e8b)

[shapeless-guide](https://books.underscore.io/shapeless-guide/shapeless-guide.html)


Official jdk images
[corretto](https://hub.docker.com/_/amazoncorretto)
[temurin](https://hub.docker.com/_/eclipse-temurin)


Note that `UUID.randomUUID` is cryptographically strong, using `SecureRandom` under the hood. This also means that it shares the issues of SecureRandom — the small possibility that the thread underneath gets blocked when generating random UUIDs.This only happens under certain conditions, however, when we generate UUIDs, we generate a lot of them, in horizontally scaled nodes being started as Docker instances, so the likelihood of hitting this in production increases.

```scala


// Scala code
import cats.effect.IO
import java.util.UUID

object UUIDUtils {
  /**
    * `UUID.randomUUID` is a risky operation, as it can block the current thread.
    * This function redirects such calls to the thread-pool meant for blocking IO.
    */
  def generateRandomUUID: IO[UUID] =
    IO.blocking(UUID.randomUUID())
}
```

A method is a member of a scope (class, object ..) declared using `def`

A `value` is an instance of a type. The type determines how we can use the value

A function value is an instance of a function type, for example
`val f:Int=>String = ...`  `f` is a function value because its type is the function type `Int=>String`

The type `Int=>String` is a short hand for `scala.Function1[Int,String]`

if `f` is a value, then f(q) expands to f.apply(1)

A lambda is a convenient way to create an instance of a function type:
`(x:Int)=>x+1` is equivalent to :
new Function[Int,Int]{
  def apply(x:Int):Int=x+1
} 
which itself expands to :
`class anon() extends Function1[Int,Int]{
  def apply(x:Int):Int=x+1
}
new anon()


a reference to a method is not a value, but can be automatically converted into one:

```scala
def inc(x:Int)=x+1

List(1,2,3).map(inc)

```
```scala
val f :...= ...List(x)...

```
The type of f needs a polymorphic method apply as a member so we can call :

`f[Int](1)==List(1)`