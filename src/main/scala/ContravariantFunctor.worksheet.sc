

trait ContravariantFunctor[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

def function1Contravariant[C]= new ContravariantFunctor[[X]=>>Function1[X,C]] {
    override def contramap[A, B](fa: A => C)(f: B => A): B => C= fa.compose(f)
    
}


val length: String => Int = _.length
val double: Double => String = _.toString


val f1: Double => Int=function1Contravariant[Int].contramap(length)(double)

//contramap is used to transform a function length: String => Int to a new function transformed: Double => Int by "pre-applying" the double: Double => String function to the input of length. 
//The compose method is used to achieve this composition.

f1(3)
8.toDouble.toString().length()
8.toDouble