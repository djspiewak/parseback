import scala.util.{Either, Left, Right}

package object parseback {

  // internal convenience utilities
  private[parseback] type \/[+A, +B] = Either[A, B]

  private[parseback] val -\/ = Left
  private[parseback] val \/- = Right

  private[parseback] def trace(str: String): Unit = {
    // println(str)
  }

  // provides 676 possible labels; should be enough for most practical purposes
  private[parseback] lazy val PossibleLabels =
    ('A' to 'Z') flatMap { a => ('A' to 'Z') map { b => a.toString + b.toString } }

  // external syntax

  type ~[+A, +B] = (A, B)
  val ~ = Tuple2

  implicit def literal(str: String): Parser[String] =
    Parser.Literal(str, 0)

  implicit def unit(u: Unit): Parser[Unit] =
    Parser.Epsilon(() :: Nil)

  final implicit class LazyParserSyntax[A](self: => Parser[A]) {
    def |(that: => Parser[A]): Parser[A] = Parser.Union(() => self, () => that)
  }
}
