//A polymorphic function type is a function type which accepts type parameters. For example:

// A polymorphic method:
def foo[A](xs: List[A]): List[A] = xs.reverse

// A polymorphic function value:
//val bar: [A] => List[A] => List[A]
//       ^^^^^^^^^^^^^^^^^^^^^^^^^
//       a polymorphic function type
 //      = [A] => (xs: List[A]) => foo[A](xs)


//This type describes function values which take a type A as a parameter, then take a list of type List[A], and return a list of the same type List[A].

 