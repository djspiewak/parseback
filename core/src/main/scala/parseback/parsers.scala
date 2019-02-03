/*
 * Copyright 2018 Daniel Spiewak
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

import util.Catenable

import scala.annotation.unchecked.uncheckedVariance
import scala.util.matching.{Regex => SRegex}
import scala.util.{Either, Left, Right}

import MemoTable.ParserId

sealed trait Parser[+A] {

  // these are not volatile since the common case is not crossing thread boundaries
  // the consequence of a thread-local cache miss is simply recomputation
  protected var nullableMemo: Nullable = Nullable.Maybe

  // the following fields are used by FieldMemoTable to avoid hashmap usage
  // they should NEVER be accessed directly; only through the MemoTable abstraction

  // true if this is a shared parser (and thus not thread-local)
  private[parseback] var isRoot: Boolean = false

  private[parseback] var derivedTable: FieldMemoTable = _
  private[parseback] var derivedC: Token = _
  private[parseback] var derivedR: Parser[A @uncheckedVariance] = _

  private[parseback] var finishedTable: FieldMemoTable = _
  private[parseback] var finished: Results.Cacheable[A @uncheckedVariance] = _

  def map[B](f: A => B): Parser[B] =
    Parser.apply(this, { (_, a: A) => Catenable(f(a)) })

  def mapWithLines[B](f: (List[Line], A) => B): Parser[B] =
    Parser.apply(this, { (line, a: A) => Catenable(f(line, a)) })

  final def map2[B, C](that: Parser[B])(f: (A, B) => C): Parser[C] = (this ~ that) map f.tupled

  def filter(p: A => Boolean): Parser[A] = Parser.Filter(this, false, p)

  // filter out but at least leave one element.
  def filterLeaveOne(p: A => Boolean): Parser[A] = Parser.Filter(this, true, p)

  final def ~[B](that: Parser[B]): Parser[A ~ B] =
    Parser.sequence(this, that)

  final def ~>[B](that: Parser[B]): Parser[B] =
    (this ~ that) map { case _ ~ b => b }

  final def <~[B](that: Parser[B]): Parser[A] =
    (this ~ that) map { case a ~ _ => a }

  def ^^^[B](b: B): Parser[B] = map { _ => b }

  // ebnf operators

  def ?(): Parser[Option[A]] =
    Parser.Epsilon(None) | (this map { Some(_) })

  def *(): Parser[List[A]] = {
    lazy val back: Parser[List[A]] = (
        this ~ back   ^^ { (_, h, t) => h :: t }
      | Parser.Epsilon(())           ^^^ Nil
    )

    back
  }

  def +(): Parser[List[A]] = {
    lazy val back: Parser[List[A]] = (
        this ~ back   ^^ { (_, h, t) => h :: t }
      | this          ^^ { (_, h) => h :: Nil }
    )

    back
  }

  /**
   * Among other things, it should be possible to instantiate F[_] with an fs2 Pull,
   * which should allow fully generalizable, incremental parsing on an ephemeral stream.
   * A prerequisite step would be to convert a Stream[Task, Bytes] (or whatever format and
   * effect it is in) into a Stream[Task, Line], which could then in turn be converted
   * pretty trivially into a Pull[Task, LineStream, Nothing].
   *
   * Please note that F[_] should be lazy and stack-safe.  If F[_] is not stack-safe,
   * this function will SOE on inputs with a very large number of lines.
   */
  final def apply[F[+_]: Monad](ls: LineStream[F]): F[Either[List[ParseError], Catenable[A]]] = {
    import LineStream._

    def markRoots(self: Parser[_], tracked: Set[ParserId[_]]): Set[ParserId[_]] = {
      import Parser._

      val id = new ParserId(self)

      if (self.isRoot || (tracked contains id)) {
        tracked
      } else {
        val tracked2 = tracked + id

        self.isRoot = true

        self match {
          case Sequence(left, right) =>
            val tracked3 = markRoots(left, tracked2)

            markRoots(right, tracked3)

          case self @ Union(_, _) =>
            markRoots(self.right, markRoots(self.left, tracked2))

          case Apply(target, _, _) =>
            markRoots(target, tracked2)

          case Filter(target, _, _) =>
            markRoots(target, tracked2)

          case _ => tracked
        }
      }
    }

    def inner(self: Parser[A], table: MemoTable)(ls: LineStream[F]): F[Either[List[ParseError], Catenable[A]]] = {
      ls match {
        case More(line, tail) =>
          trace(s"current line = ${line.project}")
          val derivation = self.derive(line, table.recreate())

          val ls2: F[LineStream[F]] = line.next map { l =>
            Monad[F] point More(l, tail)
          } getOrElse tail

          Monad[F].flatMap(ls2)(inner(derivation, table))

        case Empty() =>
          // don't clear prior table when we hit the end
          Monad[F] point self.finish(Set(), table).toEither
      }
    }

    markRoots(this, Set.empty)

    inner(this, new InitialMemoTable)(ls)
  }

  protected[parseback] final def isNullable: Boolean = {
    import Nullable._
    import Parser._

    def inner(p: Parser[_], tracked: Set[ParserId[_]]): Nullable = {
      val pid = new ParserId(p)

      if ((p.isInstanceOf[Union[_]] && (tracked contains pid)) || p.nullableMemo != Maybe) {
        p.nullableMemo
      } else {
        p match {
          case p @ Sequence(left, right) =>
            p.nullableMemo = inner(left, tracked) && inner(right, tracked)
            p.nullableMemo

          case p @ Union(_, _) =>
            val tracked2 = tracked + pid

            p.nullableMemo = inner(p.left, tracked2) || inner(p.right, tracked2)
            p.nullableMemo

          case p @ Apply(target, _, _) =>
            p.nullableMemo = inner(target, tracked)
            p.nullableMemo

          case p @ Filter(target, _, _) =>
            p.nullableMemo = inner(target, tracked)
            p.nullableMemo

          // the following four cases should never be hit, but they
          // are correctly defined here for documentation purposes
          case p @ Literal(_) =>
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

    if (nullableMemo == Maybe) {
      inner(this, Set()) match {
        case True => true
        case False => false

        /*
         * Ok, this gets a little complicated...
         *
         * If we're "stuck", that means there is no more short-
         * circuiting, no more undelegating, no more anything
         * that can be done.  So we cannot have any terms of the
         * form Or(true, X) or And(false, X), since those terms
         * immediately short-circuit.  Furthermore, being stuck
         * also requires a system of equations which is
         * intrinsically recursive in some way.  Thus, we could
         * make progress by picking a non-Solved/Delegate variable
         * and hypothesizing it has a particular truth value, and
         * the system would eventually loop back around to
         * calculating a value for that original value derived
         * from the hypothesis.  Presumably, any hypothesis which
         * does not derive a contradiction would be a valid
         * solution.
         *
         * Critically though, we do not have a negation constraint.
         * The only way that a contradiction can be derived is if
         * there exists some way to build a cyclic path which
         * "flips" a hypothesis to its negation.  There are only
         * two forms in our algebra which can achieve on some input:
         * Or(true, X) and And(false, X), with the former flipping
         * false to true, and the latter flipping true to false.
         * However, we've already ruled out the existence of those
         * terms since we are stuck in the first place, meaning that
         * there would be no hypothesis which could derive to a
         * contradiction by any stuck cycle.  Either true or false
         * would be valid answers.
         *
         * At this point, we take a step back and look at the
         * broader semantic picture.  An answer of "true" in the case
         * of an intrinsically recursive constraint set would mean
         * that the `finish` function should be able to produce a
         * non-error result given enough "looping".  This is clearly
         * incorrect.  In fact, in the semantics of PWD, any
         * intrinsically set of variables must ALL correspond to
         * false in the question of nullability, as `finish` would
         * discover when it runs into the cycles.
         */
        case Maybe =>
          nullableMemo = False
          false
      }
    } else {
      nullableMemo.toBoolean
    }
  }

  // memoized version of derive
  protected final def derive(line: Line, table: MemoTable): Parser[A] = {
    require(!line.isEmpty)

    trace(s"deriving $this by '${line.head}'")

    table.derive(this, line.head) getOrElse {
      val back = _derive(line, table)
      table.derived(this, line.head, back)
      back
    }
  }

  // TODO generalize to deriving in bulk, rather than by character
  protected def _derive(line: Line, table: MemoTable): Parser[A]

  protected final def finish(seen: Set[ParserId[_]], table: MemoTable): Results[A] = {
    val id = new ParserId(this)

    // non-union parsers aren't marked, so we can fast-fail them
    if (this.isInstanceOf[Parser.Union[_]] && (seen contains id)) {
      Results.Hypothetical(ParseError.UnboundedRecursion(this) :: Nil)
    } else {
      table finish this getOrElse {
        val seen2 = this match {
          case Parser.Union(_, _) => seen + id
          case _ => seen
        }

        val back = _finish(seen2, table)

        back match {
          case back: Results.Cacheable[A] => table.finished(this, back)
          case _ => ()
        }

        trace(s"finished fresh calculation $this => $back")

        back
      }
    }
  }

  protected def _finish(seen: Set[ParserId[_]], table: MemoTable): Results[A]
}

/*
 * Smart constructors are provided for Sequence and Apply
 */
object Parser {

  implicit val applicative: Applicative[Parser] = new Applicative[Parser] {
    def pure[A](a: A): Parser[A] = Epsilon(a)
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = (ff map2 fa) { _(_) }
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa map f
  }

  /**
   * Two sequentialized parsers with optionally interleaving whitespace.
   */
  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B] {
    nullableMemo = left.nullableMemo && right.nullableMemo

    protected def _derive(line: Line, table: MemoTable): Parser[A ~ B] = {
      trace(s">> deriving $this")

      val nonNulled = sequence(left.derive(line, table), right)

      if (left.isNullable) {
        trace(s"  >> left is nullable")

        left.finish(Set(), table).toEither match {

          /*
           * Failure is nullable, but it will return a left when finished
           */
          case Left(_) => nonNulled

          case Right(results) =>
            val nulled = Parser.apply(right.derive(line, table), { (_, b: B) =>
              results map { (_, b) }
            })

            nonNulled | nulled
        }
      } else {
        trace(s"  >> left is not nullable")
        nonNulled
      }
    }

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) = {
      left.finish(seen, table) && right.finish(seen, table)
    }
  }

  private[parseback] def sequence[A, B](
      left: Parser[A],
      right: Parser[B]): Parser[A ~ B] = {

    left match {
      case Sequence(innerLeft, innerRight) =>
        sequence(innerLeft, sequence(innerRight, right)) map {
          case (a, (b, c)) => ((a, b), c)
        }

      case left: Apply[e, A] =>
        val f: (List[Line], (e, B)) => Catenable[(A, B)] = { (lines, pair) =>
          val (e, b) = pair

          left.f(lines, e) map { (_, b) }
        }

        apply(sequence(left.target, right), f, left.lines)

      case Epsilon(value) =>
        right map { (value, _) }

      case f @ Failure(_) => f

      case _ => Sequence(left, right)
    }
  }

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Parser[A] {
    lazy val left = _left()
    lazy val right = _right()

    protected def _derive(line: Line, table: MemoTable): Parser[A] =
      left.derive(line, table) | right.derive(line, table)

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      left.finish(seen, table) || right.finish(seen, table)
  }

  final case class Apply[A, +B](target: Parser[A], f: (List[Line], A) => Catenable[B], lines: Vector[Line] = Vector.empty) extends Parser[B] {
    nullableMemo = target.nullableMemo

    protected def _derive(line: Line, table: MemoTable): Parser[B] =
      Parser.apply(target.derive(line, table), f, Line.addTo(lines, line))

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      target.finish(seen, table) pmap { as => as flatMap { f(lines.toList, _) } }
  }

  private[parseback] def apply[A, B](
      target: Parser[A],
      f: (List[Line], A) => Catenable[B],
      lines: Vector[Line] = Vector.empty): Parser[B] = {

    target match {
      // TODO we could reenable this rule if Epsilon contained a Catenable
      // case Epsilon(value) => Epsilon(f(lines.toList, Catenable(value)))

      case target: Apply[e, A] =>
        val composed: (List[Line], e) => Catenable[B] = { (lines, e) =>
          target.f(lines, e) flatMap { f(lines, _) }
        }

        apply(target.target, composed, target.lines.foldLeft(lines)(Line.addTo))

      case target @ Failure(_) => target

      case target => Apply(target, f, lines)
    }
  }

  final case class Filter[A](target: Parser[A], leaveOne: Boolean, p: A => Boolean) extends Parser[A] {
    nullableMemo = target.nullableMemo

    override def filter(p2: A => Boolean): Parser[A] =
      Filter(target, leaveOne, { a: A => p(a) && p2(a) })

    protected def _derive(line: Line, table: MemoTable) =
      Filter(target.derive(line, table), leaveOne, p)

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      // if the Result is unique, no need to filter out.
      target.finish(seen, table) pmap { c => if(leaveOne && c.length == 1) c else c filter p }
  }

  final case class Literal(literal: String) extends Parser[String] {
    nullableMemo = Nullable.False

    protected def _derive(line: Line, table: MemoTable): Parser[String] = {
      if (literal == line.head.value) {
          Epsilon(literal)
      } else {
        Failure(ParseError.UnexpectedCharacter(line, Set(literal)) :: Nil)
      }
    }

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      Results.Failure(ParseError.UnexpectedEOF(Set(literal)) :: Nil)
  }

  // note that regular expressions cannot cross line boundaries
  final case class Regex(r: SRegex) extends Parser[String] {
    require(!r.pattern.matcher("").matches)

    nullableMemo = Nullable.False

    protected def _derive(line: Line, table: MemoTable): Parser[String] = {
      val m = r findPrefixOf line.head.value

      val success = m map { lit =>
        Epsilon(lit)
      }

      success getOrElse Failure(ParseError.UnexpectedCharacter(line, Set(r.toString)) :: Nil)
    }

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      Results.Failure(ParseError.UnexpectedEOF(Set(r.toString)) :: Nil)
  }

  final case class Epsilon[+A](value: A) extends Parser[A] {
    nullableMemo = Nullable.True

    override def ^^^[B](b: B): Parser[B] = Epsilon(b)

    protected def _derive(line: Line, table: MemoTable): Parser[A] =
      Failure(ParseError.UnexpectedTrailingCharacters(line) :: Nil)

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      Results.Success(Catenable(value))
  }

  final case class Failure(errors: List[ParseError]) extends Parser[Nothing] {
    nullableMemo = Nullable.True

    protected def _derive(line: Line, table: MemoTable): Parser[Nothing] = this

    protected def _finish(seen: Set[ParserId[_]], table: MemoTable) =
      Results.Failure(errors)
  }
}
