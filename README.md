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

`erasedValue[T]`  and `constValue[]`
[type class derivation](https://medium.com/riskified-technology/type-class-derivation-in-scala-3-ba3c7c41d3ef)
[Scala3](http://www.limansky.me/posts/2021-07-26-from-scala-2-shapeless-to-scala-3.html)
