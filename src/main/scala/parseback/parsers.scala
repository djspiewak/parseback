package parseback

import cats.{Applicative, Monad}
import cats.instances.either._
import cats.instances.option._
import cats.instances.tuple._
import cats.syntax.all._

sealed trait Parser[+A] {

  // non-volatile on purpose!  parsers are not expected to cross thread boundaries during a single step
  private[this] var lastDerivation: (Char, ParseError \/ Parser[A]) = _
  private[this] var finishMemo: Option[List[A]] = _

  protected var nullableMemo: Nullable = Nullable.Maybe   // future optimization idea: Byte {-1, 0, 1} to shrink object map

  def map[B](f: A => B): Parser[B] = Parser.Reduce(this, { (_, a: A) => f(a) :: Nil })

  def map2[B, C](that: Parser[B])(f: (A, B) => C): Parser[C] = (this ~ that) map f.tupled

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

  protected final def isNullable: Boolean = {
    import Nullable._
    import Parser._

    def inner(p: Parser[_], tracked: Set[Parser[_]] = Set()): Nullable = {
      if ((tracked contains p) || p.nullableMemo != Maybe) {
        p.nullableMemo
      } else {
        p match {
          case p @ Sequence(left, right) =>
            val tracked2 = tracked + p

            p.nullableMemo = inner(left, tracked2) && inner(right, tracked2)
            p.nullableMemo

          case p @ Union(_, _) =>
            val tracked2 = tracked + p

            p.nullableMemo = inner(p.left, tracked2) || inner(p.right, tracked2)
            p.nullableMemo

          case p @ Reduce(target, _) =>
            p.nullableMemo = inner(target, tracked + p)
            p.nullableMemo

          // the following two cases should never be hit, but they
          // are correctly defined here for documentation purposes
          case p @ Literal(_, _) =>
            p.nullableMemo = False
            False

          case p @ Epsilon(_) =>
            p.nullableMemo = True
            True
        }
      }
    }

    if (nullableMemo == Maybe)
      inner(this).toBoolean
    else
      nullableMemo.toBoolean
  }

  // memoized version of derive
  protected final def derive(line: Line): ParseError \/ Parser[A] = {
    val snapshot = lastDerivation

    if (snapshot != null && snapshot._1 == line.head) {
      snapshot._2
    } else {
      val back = _derive(line)
      lastDerivation = line.head -> back

      back
    }
  }

  // TODO generalize to deriving in bulk, rather than by character
  protected def _derive(line: Line): ParseError \/ Parser[A]

  protected final def finish: Option[List[A]] = {
    if (finishMemo == null) {
      val back = _finish
      finishMemo = back

      back
    } else {
      finishMemo
    }
  }

  /**
   * If isNullable == false, then finish == None.
   */
  protected def _finish: Option[List[A]]
}

object Parser {

  implicit val applicative: Applicative[Parser] = new Applicative[Parser] {
    def pure[A](a: A): Parser[A] = Epsilon(a)
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = (ff map2 fa) { _(_) }
  }

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

    protected def _finish =
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

    protected def _finish = {
      val lf = left.finish
      val rf = right.finish
      val both = (lf map2 rf) { _ ++ _ }

      both orElse lf orElse rf
    }
  }

  final case class Reduce[A, +B](target: Parser[A], f: (List[Line], A) => List[B]) extends Nonterminal[B] {

    protected def _derive(line: Line): ParseError \/ Parser[B] =
      target derive line map { Reduce(_, f) }

    protected def _finish = target.finish map { rs => rs flatMap { f(Nil, _) } }   // TODO line accumulation used here!
  }

  final case class Literal(literal: String, offset: Int = 0) extends Terminal[String] {
    require(literal.length > 0)
    require(offset < literal.length)

    nullableMemo = Nullable.False

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

    protected def _finish = None
  }

  final case class Epsilon[+A](value: A) extends Terminal[A] {
    nullableMemo = Nullable.True

    protected def _derive(line: Line): ParseError \/ Parser[A] =
      -\/(ParseError.UnexpectedTrailingCharacters(line))

    protected def _finish = Some(value :: Nil)
  }
}
