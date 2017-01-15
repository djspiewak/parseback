/*
 * Copyright 2017 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package parseback

import cats.{Applicative, Monad}
import cats.instances.either._
import cats.syntax.all._

import scala.util.matching.{Regex => SRegex}

sealed trait Parser[+A] {

  // non-volatile on purpose!  parsers are not expected to cross thread boundaries during a single step
  private[this] var lastDerivation: (Line, Parser[A]) = _
  private[this] var finishMemo: List[A] = _     // cannot memoize failure

  protected var nullableMemo: Nullable = Nullable.Maybe

  def map[B](f: A => B): Parser[B] = Parser.Apply(this, { (_, a: A) => f(a) :: Nil })

  def mapWithLines[B](f: (List[Line], A) => B): Parser[B] =
    Parser.Apply(this, { (line, a: A) => f(line, a) :: Nil })

  final def map2[B, C](that: Parser[B])(f: (A, B) => C): Parser[C] = (this ~ that) map f.tupled

  final def ~[B](that: Parser[B]): Parser[A ~ B] = Parser.Sequence(this, that)

  final def ~>[B](that: Parser[B]): Parser[B] =
    (this ~ that) map { case _ ~ b => b }

  final def <~[B](that: Parser[B]): Parser[A] =
    (this ~ that) map { case a ~ _ => a }

  def ^^^[B](b: B): Parser[B] = map { _ => b }

  /**
   * Among other things, it should be possible to instantiate F[_] with an fs2 Pull,
   * which should allow fully generalizable, incremental parsing on an ephemeral stream.
   * A prerequisite step would be to convert a Stream[Task, Bytes] (or whatever format and
   * effect it is in) into a Stream[Task, Line], which could then in turn be converted
   * pretty trivially into a Pull[Task, LineStream, Nothing].
   */
  final def apply[F[+_]: Monad](ls: LineStream[F]): F[List[ParseError] \/ List[A]] = {
    ls.normalize flatMap {
      case LineStream.More(line, tail) =>
        trace(s"current line = ${line.project}")
        val derivation = derive(line)

        val ls2: F[LineStream[F]] = line.next map { l =>
          Monad[F].pure(LineStream.More(l, tail))
        } getOrElse tail

        ls2 flatMap { derivation(_) }

      case LineStream.Empty() =>
        Monad[F].pure(finish(Set()))
    }
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

          case p @ Apply(target, _, _) =>
            p.nullableMemo = inner(target, tracked + p)
            p.nullableMemo

          // the following two cases should never be hit, but they
          // are correctly defined here for documentation purposes
          case p @ Literal(_, _) =>
            p.nullableMemo = False
            False

          case p @ Regex(_) =>
            p.nullableMemo = False
            False

          case p @ Epsilon(_) =>
            p.nullableMemo = True
            True

          case p @ Failure(_) =>
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
  protected final def derive(line: Line): Parser[A] = {
    assert(!line.isEmpty)

    val snapshot = lastDerivation

    if (snapshot != null && snapshot._1 == line) {
      snapshot._2
    } else {
      val back = _derive(line)
      lastDerivation = line -> back

      back
    }
  }

  // TODO generalize to deriving in bulk, rather than by character
  protected def _derive(line: Line): Parser[A]

  protected final def finish(seen: Set[Parser[_]]): List[ParseError] \/ List[A] = {
    if (seen contains this) {
      -\/(ParseError.UnboundedRecursion(this) :: Nil)
    } else if (finishMemo == null) {
      val back = _finish(seen + this)

      back foreach { results =>
        finishMemo = results
      }

      back
    } else {
      \/-(finishMemo)
    }
  }

  /**
   * If isNullable == false, then finish == None.
   */
  protected def _finish(seen: Set[Parser[_]]): List[ParseError] \/ List[A]
}

object Parser {

  implicit val applicative: Applicative[Parser] = new Applicative[Parser] {
    def pure[A](a: A): Parser[A] = Epsilon(a)
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = (ff map2 fa) { _(_) }
  }

  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B] {

    protected def _derive(line: Line): Parser[A ~ B] = {
      if (left.isNullable) {
        lazy val nonNulled = left.derive(line) ~ right

        lazy val nulled = left.finish(Set()) match {
          case -\/(errs) => Failure(errs)     // take the left errors
          case \/-(results) =>
            Apply(right.derive(line), { (_, b: B) => results map { (_, b) } })
        }

        nonNulled | nulled
      } else {
        left.derive(line) ~ right
      }
    }

    protected def _finish(seen: Set[Parser[_]]) =
      for {
        lf <- left finish seen
        rf <- right finish seen
      } yield lf flatMap { l => rf map { r => (l, r) } }
  }

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Parser[A] {
    lazy val left = _left()
    lazy val right = _right()

    protected def _derive(line: Line): Parser[A] =
      left.derive(line) | right.derive(line)

    protected def _finish(seen: Set[Parser[_]]) = {
      val lf = left finish seen
      val rf = right finish seen
      val both = (lf map2 rf) { _ ++ _ }

      // TODO correctly merge errors in the event that both sides fail
      both orElse lf orElse rf
    }
  }

  final case class Apply[A, +B](target: Parser[A], f: (List[Line], A) => List[B], lines: Vector[Line] = Vector.empty) extends Parser[B] {

    override def map[C](f2: B => C): Parser[C] =
      Apply(target, { (lines, a: A) => f(lines, a) map f2 }, lines)

    override def mapWithLines[C](f2: (List[Line], B) => C): Parser[C] =
      Apply(target, { (lines, a: A) => f(lines, a) map { f2(lines, _) } }, lines)

    protected def _derive(line: Line): Parser[B] =
      Apply(target derive line, f, lines :+ line)

    protected def _finish(seen: Set[Parser[_]]) =
      target finish seen map { rs => rs flatMap { f(lines.toList, _) } }
  }

  final case class Literal(literal: String, offset: Int = 0)(implicit W: Whitespace) extends Parser[String] {
    require(literal.length > 0)
    require(offset < literal.length)

    nullableMemo = Nullable.False

    protected def _derive(line: Line): Parser[String] = {
      W stripLeading line match {
        case Some(_) =>
          this

        case None =>
          if (literal.charAt(offset) == line.head) {
            if (offset == literal.length - 1)
              Epsilon(literal)
            else
              Literal(literal, offset + 1)
          } else {
            Failure(ParseError.UnexpectedCharacter(line) :: Nil)
          }
      }
    }

    protected def _finish(seen: Set[Parser[_]]) =
      -\/(ParseError.UnexpectedEOF :: Nil)
  }

  // note that regular expressions cannot cross line boundaries
  final case class Regex(r: SRegex)(implicit W: Whitespace) extends Parser[String] {
    require(!r.pattern.matcher("").matches)

    nullableMemo = Nullable.False

    protected def _derive(line: Line): Parser[String] = {
      W stripLeading line match {
        case Some(line2) =>
          val m = r findPrefixOf line2.project

          val success = m map { Literal(_) }

          success getOrElse Failure(ParseError.UnexpectedCharacter(line2) :: Nil)

        case None =>
          val m = r findPrefixOf line.project

          val success = m map { lit =>
            if (lit.length == 1)
              Epsilon(lit)
            else
              Literal(lit, 1)(Whitespace.Default)     // don't re-strip within a token
          }

          success getOrElse Failure(ParseError.UnexpectedCharacter(line) :: Nil)
      }
    }

    protected def _finish(seen: Set[Parser[_]]) =
      -\/(ParseError.UnexpectedEOF :: Nil)
  }

  final case class Epsilon[+A](value: A) extends Parser[A] {
    nullableMemo = Nullable.True

    override def ^^^[B](b: B): Parser[B] = Epsilon(b)

    protected def _derive(line: Line): Parser[A] =
      Failure(ParseError.UnexpectedTrailingCharacters(line) :: Nil)

    protected def _finish(seen: Set[Parser[_]]) = \/-(value :: Nil)
  }

  final case class Failure(errors: List[ParseError]) extends Parser[Nothing] {
    nullableMemo = Nullable.True

    protected def _derive(line: Line): Parser[Nothing] = this

    protected def _finish(seen: Set[Parser[_]]) = -\/(errors)
  }
}
