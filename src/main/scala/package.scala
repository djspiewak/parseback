import scala.util.{Either, Left, Right}

package object parseback {
  type ~[+A, +B] = (A, B)
  val ~ = Tuple2

  private[parseback] type \/[+A, +B] = Either[A, B]

  private[parseback] def -\/[A, B](a: A): Either[A, B] = Left(a)
  private[parseback] def \/-[A, B](b: B): Either[A, B] = Right(b)

  final implicit class LazyParserSyntax[A](self: => Parser[A]) {
    def |(that: => Parser[A]): Parser[A] = Parser.Union(() => self, () => that)
  }
}
