import scala.collection.Factory
import scala.deriving.Mirror

trait Transformer[From, To] {
  def transform(from: From): To

}
object Transformer {
  def apply[A, B](using transformer: Transformer[A, B]): Transformer[A, B] =
    transformer

  given [A]: Transformer[A, A]                                        =
    new {
      def transform(from: A): A = from
    }
  given [A, B](using t: Transformer[A, B]): Transformer[A, Option[B]] =
    new {
      def transform(from: A): Option[B] =
        t.transform.andThen(Some.apply)(from)
    }

  given [A, B](using t: Transformer[A, B]): Transformer[Option[A], Option[B]] =
    new {
      def transform(from: Option[A]): Option[B] =
        from.map(t.transform)
    }

  given [A, B, CollFrom[+elem] <: Iterable[elem], CollTo[+elem] <: Iterable[
    elem
  ]](using
      transformer: Transformer[A, B],
      factory: Factory[B, CollTo[B]]
  ): Transformer[CollFrom[A], CollTo[B]] =
    new {
      def transform(from: CollFrom[A]): CollTo[B] =
        from
          .foldLeft(factory.newBuilder)(_ += transformer.transform(_))
          .result

    }

  extension [A <: Product](from: A) {
    def to[B](using Transformer[A, B]): B = Transformer[A, B].transform(from)

    inline def into[B <: Product](using
        A: Mirror.ProductOf[A],
        B: Mirror.ProductOf[B]
    ) = ??? // TransformerBuilder.create(from)(using A, B)

  }

  sealed trait Field[Label <: String, Type]
  type FromLabelsAndTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
    (Labels, Types) match {
      case (EmptyTuple, EmptyTuple)                       => EmptyTuple
      case (labelHead *: labelTail, typeHead *: typeTail) =>
        Field[labelHead, typeHead] *: FromLabelsAndTypes[labelTail, typeTail]
    }
}
