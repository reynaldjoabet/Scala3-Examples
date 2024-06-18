trait Show[A] {
  def show(value: A): String
}

object Show {

  implicit val intShow: Show[Int] = new Show[Int] {
    def show(value: Int): String = value.toString
  }

  implicit val stringShow: Show[String] = new Show[String] {
    def show(value: String): String = value
  }

}

trait Printable[A] {
  def print(value: A): String
}

object Printable {}

implicit def printableShow[A](implicit showInstance: Show[A]): Printable[A] =
  new Printable[A] {
    def print(value: A): String = showInstance.show(value)
  }

def printAndShow[A](
  value: A
)(implicit printableInstance: Printable[A]): Unit =
  println(printableInstance.print(value))

printAndShow(42)              // Uses the implicit Show[Int] to convert to String
printAndShow("Hello, Scala!") //// Uses the implicit Show[String] to get the value

//Type class derivation involves automatically deriving type class instances for certain types. Libraries like Magnolia or Shapeless provide mechanisms for automatic derivation