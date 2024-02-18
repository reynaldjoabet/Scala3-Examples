import scala.deriving._
import scala.compiletime._
enum Tree[T] derives Show, CanEqual {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}
trait Show[A]                       {
  def show(a: A): String
}
object Show                         {

  inline def derived[T](using m: Mirror.Of[T]): Show[T] =
    new Show[T] {
      override def show(a: T): String = {
        inline m match {
          case s: Mirror.SumOf[T]     => showSum(s, a)
          case p: Mirror.ProductOf[T] => showProduct(p, a)
        }
      }
    }

  inline given derivedArray[A](using elemShow: Show[A]): Show[Array[A]] =
    new Show[Array[A]] {
      def show(value: Array[A]): String =
        value.toList.map(elemShow.show).mkString(",")
    }
  inline def elemLabels[T <: Tuple]: List[String]                       =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].asInstanceOf[String] :: elemLabels[ts]
    }

  inline def summonAll[T <: Tuple]: List[Show[?]] =
    inline erasedValue[T] match {
      case EmptyTuple   => Nil
      case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]

    }

  inline def showProduct[A](m: Mirror.ProductOf[A], a: A): String = {
    // m.MirroredLabel returns the singleton type eg Person.type
    // in order to get the value that has that type, we use constValue
    val productName: String =
      constValue[m.MirroredLabel] // something like "Person"
    val fieldNames               = elemLabels[m.MirroredElemLabels]
    val instances: List[Show[_]] =
      summonAll[m.MirroredElemTypes] // eg List(Show[Strin],Show[Int])
    val values =
      a.asInstanceOf[Product]
        .productIterator
        .toList // eg  List[Any] = List(1984, George Orwell, 1961, 328)

    val fields = (fieldNames zip (instances zip values)).map {
      case (name, (instance, value)) =>
        s"$name= ${instance.asInstanceOf[Show[Any]].show(value)}"
    }
    s"${productName}(${fields.mkString(",")})"
  }

  inline def showCase[A, T <: Tuple](n: Int, ord: Int, a: A): String = {
    inline erasedValue[T] match {
      case EmptyTuple   => ""
      case _: (t *: ts) =>
        if (n == ord) {
          summonFrom { case p: Mirror.ProductOf[`t`] =>
            showProduct(p, a.asInstanceOf[t])
          }
        } else showCase[A, ts](n + 1, ord, a)
    }
  }
  inline def showSum[A](m: Mirror.SumOf[A], a: A): String            = {
    val ord = m.ordinal(a)
    showCase[A, m.MirroredElemTypes](0, ord, a)
  }
}
