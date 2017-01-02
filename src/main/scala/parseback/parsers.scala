package parseback

import cats.Monad
import cats.instances.either._
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.all._

trait Parser[+A] {

  @volatile
  private[this] var lastDerivation: (Line, ParseError \/ Parser[A]) = _

  def map[B](f: A => B): Parser[B] = Parser.Reduce(this, { (_, a: A) => f(a) :: Nil })

  def ~[B](that: Parser[B]): Parser[A ~ B] = Parser.Sequence(this, that)

  // TODO diversify with associative ~ deconstruction by arity
  def ^^[B](f: (List[Line], A) => B): Parser[B] =
    Parser.Reduce(this, { (line, a: A) => f(line, a) :: Nil })

  /**
   * Among other things, it should be possible to instantiate F[_] with an fs2 Pull,
   * which should allow fully generalizable, incremental parsing on an ephemeral stream.
   * A prerequisite step would be to convert a Stream[Task, Bytes] (or whatever format and
   * effect it is in) into a Stream[Task, Line], which could then in turn be converted
   * pretty trivially into a Pull[Task, LineStream, Nothing].
   */
  def apply[F[+_]: Monad](ls: LineStream[F]): F[ParseError \/ List[A]] = ls match {
    case LineStream.More(line, tail) =>
      val derivation = derive(line)

      derivation traverse { self2 =>
        val ls2: F[LineStream[F]] = line.next map { l =>
          Monad[F].pure(LineStream.More(l, tail))
        } getOrElse tail

        ls2 flatMap { self2(_) }
      } map { _.flatten }

    case LineStream.Empty() =>
      val back = finish map { \/-(_) } getOrElse -\/(ParseError.UnexpectedEOF)
      Monad[F].pure(back)
  }

  // TODO
  protected def isNullable: Boolean = false

  // memoized version of derive
  protected final def derive(line: Line): ParseError \/ Parser[A] = {
    val snapshot = lastDerivation

    if (snapshot != null && (snapshot._1 eq line)) {
      snapshot._2
    } else {
      val back = _derive(line)
      lastDerivation = line -> back

      back
    }
  }

  // TODO generalize to deriving in bulk, rather than by character
  protected def _derive(line: Line): ParseError \/ Parser[A]

  protected def finish: Option[List[A]]
}

object Parser {

  sealed trait Terminal[+A] extends Parser[A]
  sealed trait Nonterminal[+A] extends Parser[A]

  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Nonterminal[A ~ B] {

    protected def _derive(line: Line): ParseError \/ Parser[A ~ B] = {
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

    protected def finish =
      for {
        lf <- left.finish
        rf <- right.finish
      } yield lf flatMap { l => rf map { r => (l, r) } }
  }

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Nonterminal[A] {
    lazy val left = _left()
    lazy val right = _right()

    protected def _derive(line: Line): ParseError \/ Parser[A] = {
      val ld = left derive line
      val rd = right derive line

      val both = ((ld, rd).bisequence map { case (l, r) => l | r } )

      both orElse ld orElse rd
    }

    protected def finish = {
      val lf = left.finish
      val rf = right.finish
      val both = (lf map2 rf) { _ ++ _ }

      both orElse lf orElse rf
    }
  }

  final case class Reduce[A, +B](target: Parser[A], f: (List[Line], A) => List[B]) extends Nonterminal[B] {

    protected def _derive(line: Line): ParseError \/ Parser[B] =
      target derive line map { Reduce(_, f) }

    protected def finish = target.finish map { rs => rs flatMap { f(Nil, _) } }   // TODO line accumulation used here!
  }

  final case class Literal(literal: String, offset: Int = 0) extends Terminal[String] {
    require(literal.length > 0)
    require(offset < literal.length)

    protected def _derive(line: Line): ParseError \/ Parser[String] = {
      if (literal.charAt(offset) == line.head) {
        if (offset == literal.length - 1)
          \/-(Epsilon(literal))
        else
          \/-(Literal(literal, offset + 1))
      } else {
        -\/(ParseError.UnexpectedCharacter(line))
      }
    }

    protected def finish = None
  }

  final case class Epsilon[+A](value: A) extends Terminal[A] {

    protected def _derive(line: Line): ParseError \/ Parser[A] =
      -\/(ParseError.UnexpectedTrailingCharacters(line))

    protected def finish = Some(value :: Nil)
  }
}
