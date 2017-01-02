import scala.util.{Either, Left, Right}

package object parseback {

  // internal convenience utilities
  private[parseback] type \/[+A, +B] = Either[A, B]

  private[parseback] val -\/ = Left
  private[parseback] val \/- = Right

  // external syntax

  type ~[+A, +B] = (A, B)
  val ~ = Tuple2

  implicit def literal(str: String): Parser[String] =
    Parser.Literal(str, 0)

  implicit def unit(u: Unit): Parser[Unit] =
    Parser.Epsilon(())

  final implicit class LazyParserSyntax[A](self: => Parser[A]) {
    def |(that: => Parser[A]): Parser[A] = Parser.Union(() => self, () => that)
  }
}
