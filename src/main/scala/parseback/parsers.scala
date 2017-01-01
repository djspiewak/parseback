package parseback

trait Parser[+A] {

  def map[B](f: A => B): Parser[B] = Parser.Derive(this, { (_, a: A) => f(a) })

  def ~[B](that: Parser[B]): Parser[A ~ B] = Parser.Sequence(this, that)

  // TODO diversify with associative ~ deconstruction by arity
  def ^^[B](f: (Line, A) => B): Parser[B] = Parser.Derive(this, f)

  def apply[F[+_]](ls: LineStream[F]): F[A] = ???
}

object Parser {

  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B]

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Parser[A] {
    lazy val left = _left()
    lazy val right = _right()
  }

  final case class Derive[A, +B](target: Parser[A], f: (Line, A) => B) extends Parser[B]

  final case class Literal(literal: String) extends Parser[String]
  case object Epsilon extends Parser[Unit]
}
