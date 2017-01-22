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

import shims.{Applicative, Monad}

import scala.util.matching.{Regex => SRegex}

import util.EitherSyntax._

sealed trait Parser[+A] {

  protected var nullableMemo: Nullable = Nullable.Maybe

  def map[B](f: A => B): Parser[B] = Parser.Apply(this, { (_, a: A) => f(a) :: Nil })

  def mapWithLines[B](f: (List[Line], A) => B): Parser[B] =
    Parser.Apply(this, { (line, a: A) => f(line, a) :: Nil })

  final def map2[B, C](that: Parser[B])(f: (A, B) => C): Parser[C] = (this ~ that) map f.tupled

  final def ~[B](that: Parser[B]): Parser[A ~ B] = Parser.Sequence(this, that)

  final def ~(): Parser[A ~ Unit] = Parser.Sequence(this, unit(()))

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
   *
   * Please note that F[_] should be lazy and stack-safe.  If F[_] is not stack-safe,
   * this function will SOE on inputs with a very large number of lines.
   */
  final def apply[F[+_]: Monad](ls: LineStream[F])(implicit W: Whitespace): F[List[ParseError] \/ List[A]] = {
    import LineStream._

    def stripTrailing(r: SRegex)(ls: LineStream[F]): F[LineStream[F]] = ls match {
      case More(line, tail) =>
        Monad[F].map(tail) {
          case ls @ More(_, _) =>
            More(line, stripTrailing(r)(ls))

          case Empty() =>
            val base2 = r.replaceAllIn(line.base, "")
            More(Line(base2, line.lineNo, line.colNo), Monad[F] point Empty[F]())
        }

      case Empty() => Monad[F] point Empty[F]()
    }

    def inner(self: Parser[A])(ls: LineStream[F]): F[List[ParseError] \/ List[A]] = ls match {
      case More(line, tail) =>
        trace(s"current line = ${line.project}")
        val derivation = self.derive(line, new MemoTable)   // create a new memo table with each new character

        val ls2: F[LineStream[F]] = line.next map { l =>
          Monad[F] point More(l, tail)
        } getOrElse tail

        Monad[F].flatMap(ls2)(inner(derivation))

      case Empty() =>
        Monad[F] point self.finish(Set(), new MemoTable)
    }

    val recompiled =
      Some(s"""${W.regex.toString}$$"""r) filter { _ => W.regex.toString != "" }

    val stripper =
      recompiled map stripTrailing getOrElse { ls: LineStream[F] => Monad[F] point ls }

    val prepped = Monad[F].flatMap(stripper(ls)) { _.normalize }

    Monad[F].flatMap(prepped)(inner(this))
  }

  protected[parseback] final def isNullable: Boolean = {
    import Nullable._
    import MemoTable.ParserId
    import Parser._

    sealed trait Constraint extends Product with Serializable

    final case class And(left: ParserId[_], right: ParserId[_]) extends Constraint
    final case class Or(left: ParserId[_], right: ParserId[_]) extends Constraint
    final case class Delegate(delegate: ParserId[_]) extends Constraint
    final case class Solved(b: Boolean) extends Constraint

    type ConstSet = Map[ParserId[_], Constraint]

    def constraints(p: Parser[_], seen: Set[ParserId[_]]): ConstSet = {
      if (p.nullableMemo != Maybe) {
        Map(new ParserId(p) -> Solved(p.nullableMemo.toBoolean))
      } else if (seen contains (new ParserId(p))) {
        Map()
      } else {
        val self = new ParserId(p)

        p match {
          case Sequence(left, right) =>
            val here = Map(
              self -> And(new ParserId(left), new ParserId(right)))

            val rec = seen + self
            here ++ constraints(left, rec) ++ constraints(right, rec)

          case p @ Union(_, _) =>
            val here = Map(
              self -> Or(new ParserId(p.left), new ParserId(p.right)))

            val rec = seen + self
            here ++ constraints(p.left, rec) ++ constraints(p.right, rec)

          case Apply(target, _, _) =>
            val here = Map(self -> Delegate(new ParserId(target)))

            here ++ constraints(target, seen + self)

          case _ => throw new AssertionError("impossible due to nullableMemo init")
        }
      }
    }

    def undelegate(cs: ConstSet)(target: ParserId[_]): ParserId[_] = {
      val received = cs get target

      val result = received collect {
        case Delegate(target2) => target2
      } map undelegate(cs)

      result getOrElse target
    }

    def substitute(cs: ConstSet): ConstSet = cs map {
      case (id, And(left, right)) =>
        val left2 = undelegate(cs)(left)
        val right2 = undelegate(cs)(right)

        val leftSolve = cs get left2 collect { case Solved(b) => b }
        val rightSolve = cs get right2 collect { case Solved(b) => b }

        val joined = for {
          ls <- leftSolve
          rs <- rightSolve
        } yield ls && rs

        val shorted =
          (leftSolve filter { _ == false }) orElse
            (rightSolve filter { _ == false })

        val solved = joined orElse shorted

        solved foreach { b =>
          id.self.nullableMemo = if (b) True else False
        }

        id -> (solved map Solved getOrElse And(left2, right2))

      case (id, Or(left, right)) =>
        val left2 = undelegate(cs)(left)
        val right2 = undelegate(cs)(right)

        val leftSolve = cs get left2 collect { case Solved(b) => b }
        val rightSolve = cs get right2 collect { case Solved(b) => b }

        val joined = for {
          ls <- leftSolve
          rs <- rightSolve
        } yield ls || rs

        val shorted =
          (leftSolve filter { _ == true }) orElse
            (rightSolve filter { _ == true })

        val solved = joined orElse shorted

        solved foreach { b =>
          id.self.nullableMemo = if (b) True else False
        }

        id -> (solved map Solved getOrElse Or(left2, right2))

      case pair => pair
    }

    def simplify(cs: ConstSet): ConstSet = cs map {
      case (id, c @ And(left, right)) =>
        val c2 = if (left == id)
          Delegate(right)
        else if (right == id)
          Delegate(left)
        else
          c

        id -> c2

      case (id, c @ Or(left, right)) =>
        val c2 = if (left == id)
          Delegate(right)
        else if (right == id)
          Delegate(left)
        else
          c

        id -> c2

      case pair @ (id, Delegate(target)) =>
        if (id == target)
          throw new IllegalStateException(s"parser ${id.self} is infinitely recursive")
        else
          (id, cs(target))

      case pair => pair
    }

    val self = new ParserId(this)

    def solveAll(cs: ConstSet): Boolean = {
      cs(self) match {
        case Solved(b) => b
        case _ => solveAll(simplify(substitute(cs)))
      }
    }

    solveAll(constraints(this, Set()))
  }

  // memoized version of derive
  protected final def derive(line: Line, table: MemoTable): Parser[A] = {
    assert(!line.isEmpty)

    trace(s"deriving $this by '${line.head}'")

    table.derive(this, line.head) getOrElse {
      val back = _derive(line, table)
      table.derived(this, line.head, back)
      back
    }
  }

  // TODO generalize to deriving in bulk, rather than by character
  protected def _derive(line: Line, table: MemoTable): Parser[A]

  protected final def finish(seen: Set[Parser[_]], table: MemoTable): List[ParseError] \/ List[A] = {
    if (seen contains this) {
      -\/(ParseError.UnboundedRecursion(this) :: Nil)
    } else {
      val back = table.finish(this) getOrElse {
        val back = _finish(seen + this, table)
        table.finished(this, back)
        back
      }

      trace(s"finished $this => $back")

      back
    }
  }

  /**
   * If isNullable == false, then finish ~= -\/(_)
   */
  protected def _finish(seen: Set[Parser[_]], table: MemoTable): List[ParseError] \/ List[A]
}

object Parser {

  implicit val applicative: Applicative[Parser] = new Applicative[Parser] {
    def point[A](a: A): Parser[A] = Epsilon(a)
    def ap[A, B](fa: Parser[A])(ff: Parser[A => B]): Parser[B] = (ff map2 fa) { _(_) }
    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa map f
  }

  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A ~ B] {

    protected def _derive(line: Line, table: MemoTable): Parser[A ~ B] = {
      trace(s">> deriving $this")

      if (left.isNullable) {
        trace(s"  >> left is nullable")

        lazy val nonNulled = left.derive(line, table) ~ right

        left.finish(Set(), table) match {
          case -\/(_) => nonNulled
          case \/-(results) =>
            nonNulled | Apply(right.derive(line, table), { (_, b: B) => results map { (_, b) } })
        }
      } else {
        trace(s"  >> left is not nullable")
        left.derive(line, table) ~ right
      }
    }

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) =
      for {
        lf <- left.finish(seen, table)
        rf <- right.finish(seen, table)
      } yield lf flatMap { l => rf map { r => (l, r) } }
  }

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Parser[A] {
    lazy val left = _left()
    lazy val right = _right()

    protected def _derive(line: Line, table: MemoTable): Parser[A] =
      left.derive(line, table) | right.derive(line, table)

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) = {
      val lf = left.finish(seen, table)
      val rf = right.finish(seen, table)

      (lf, rf) match {
        case (-\/(lErr), -\/(rErr)) =>
          -\/(ParseError.prioritize(lErr ::: rErr))

        case (\/-(lRes), \/-(rRes)) =>
          \/-(lRes ::: rRes)

        case _ =>
          lf.fold(_ => rf, v => \/-(v))
      }
    }
  }

  final case class Apply[A, +B](target: Parser[A], f: (List[Line], A) => List[B], lines: Vector[Line] = Vector.empty) extends Parser[B] {

    override def map[C](f2: B => C): Parser[C] =
      Apply(target, { (lines, a: A) => f(lines, a) map f2 }, lines)

    override def mapWithLines[C](f2: (List[Line], B) => C): Parser[C] =
      Apply(target, { (lines, a: A) => f(lines, a) map { f2(lines, _) } }, lines)

    protected def _derive(line: Line, table: MemoTable): Parser[B] =
      Apply(target.derive(line, table), f, lines :+ line)

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) =
      target.finish(seen, table) map { rs => rs flatMap { f(lines.toList, _) } }
  }

  final case class Literal(literal: String, offset: Int = 0)(implicit W: Whitespace) extends Parser[String] {
    require(literal.length > 0)
    require(offset < literal.length)

    nullableMemo = Nullable.False

    protected def _derive(line: Line, table: MemoTable): Parser[String] = {
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
            Failure(ParseError.UnexpectedCharacter(line, Set(literal substring offset)) :: Nil)
          }
      }
    }

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) =
      -\/(ParseError.UnexpectedEOF(Set(literal substring offset)) :: Nil)
  }

  // note that regular expressions cannot cross line boundaries
  final case class Regex(r: SRegex)(implicit W: Whitespace) extends Parser[String] {
    require(!r.pattern.matcher("").useAnchoringBounds(false).matches)

    nullableMemo = Nullable.False

    protected def _derive(line: Line, table: MemoTable): Parser[String] = {
      W stripLeading line match {
        case Some(line2) =>
          val m = r findPrefixOf line2.project

          val success = m map { Literal(_) }

          success getOrElse Failure(ParseError.UnexpectedCharacter(line2, Set(r.toString)) :: Nil)

        case None =>
          val m = r findPrefixOf line.project

          val success = m map { lit =>
            if (lit.length == 1)
              Epsilon(lit)
            else
              Literal(lit, 1)(Whitespace.Default)     // don't re-strip within a token
          }

          success getOrElse Failure(ParseError.UnexpectedCharacter(line, Set(r.toString)) :: Nil)
      }
    }

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) =
      -\/(ParseError.UnexpectedEOF(Set(r.toString)) :: Nil)
  }

  final case class Epsilon[+A](value: A) extends Parser[A] {
    nullableMemo = Nullable.True

    override def ^^^[B](b: B): Parser[B] = Epsilon(b)

    protected def _derive(line: Line, table: MemoTable): Parser[A] =
      Failure(ParseError.UnexpectedTrailingCharacters(line) :: Nil)

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) = \/-(value :: Nil)
  }

  final case class Failure(errors: List[ParseError]) extends Parser[Nothing] {
    nullableMemo = Nullable.True

    protected def _derive(line: Line, table: MemoTable): Parser[Nothing] = this

    protected def _finish(seen: Set[Parser[_]], table: MemoTable) = -\/(errors)
  }
}
