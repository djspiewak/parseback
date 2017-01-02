package parseback

import cats.instances.either._
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.all._

trait Parser[+A] {

  def map[B](f: A => B): Parser[B] = Parser.Reduce(this, { (_, a: A) => f(a) :: Nil })

  def ~[B](that: Parser[B]): Parser[A ~ B] = Parser.Sequence(this, that)

  // TODO diversify with associative ~ deconstruction by arity
  def ^^[B](f: (List[Line], A) => B): Parser[B] = Parser.Reduce(this, { (line, a: A) => f(line, a) :: Nil })

  def apply[F[+_]](ls: LineStream[F]): F[List[A]] = ???

  // TODO
  def isNullable: Boolean = false

  // TODO generalize to deriving in bulk, rather than by character
  def derive(line: Line): ParseError \/ Parser[A]

  def finish: Option[List[A]]
}

object Parser {

  sealed trait Terminal[+A] extends Parser[A]
  sealed trait Nonterminal[+A] extends Parser[A]

  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Nonterminal[A ~ B] {

    def derive(line: Line): ParseError \/ Parser[A ~ B] = {
      if (left.isNullable) {
        val nonNulled = left.derive(line) map { _ ~ right }

        val nulled = right.derive(line) map { p =>
          Reduce(p, { (_, b: B) => left.finish.toList.flatten map { (_, b) } })
        }

        (nonNulled map2 nulled) { _ | _ }   // TODO this drives the laziness inside the monad, making it useless; is that ok?
      } else {
        left.derive(line) map { _ ~ right }
      }
    }

    def finish =
      for {
        lf <- left.finish
        rf <- right.finish
      } yield lf flatMap { l => rf map { r => (l, r) } }
  }

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Nonterminal[A] {
    lazy val left = _left()
    lazy val right = _right()

    def derive(line: Line): ParseError \/ Parser[A] = {
      val ld = left derive line
      val rd = right derive line

      val both = ((ld, rd).bisequence map { case (l, r) => l | r } )

      both orElse ld orElse rd
    }

    def finish = {
      val lf = left.finish
      val rf = right.finish
      val both = (lf map2 rf) { _ ++ _ }

      both orElse lf orElse rf
    }
  }

  final case class Reduce[A, +B](target: Parser[A], f: (List[Line], A) => List[B]) extends Nonterminal[B] {

    def derive(line: Line): ParseError \/ Parser[B] =
      target derive line map { Reduce(_, f) }

    def finish = target.finish map { rs => rs flatMap { f(Nil, _) } }   // TODO line accumulation used here!
  }

  final case class Literal(literal: String, offset: Int = 0) extends Terminal[String] {
    require(literal.length > 0)
    require(offset < literal.length)

    def derive(line: Line): ParseError \/ Parser[String] = {
      if (literal.charAt(offset) == line.head) {
        if (offset == literal.length - 1)
          \/-(Epsilon(literal))
        else
          \/-(Literal(literal, offset + 1))
      } else {
        -\/(ParseError.UnexpectedCharacter(line))
      }
    }

    def finish = None
  }

  final case class Epsilon[+A](value: A) extends Terminal[A] {

    def derive(line: Line): ParseError \/ Parser[A] =
      -\/(ParseError.UnexpectedTrailingCharacters(line))

    def finish = Some(value :: Nil)
  }
}
