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
import cats.instances.list._
import cats.instances.option._
import cats.data.State
import cats.syntax.all._

sealed trait Parser[+A] {

  // non-volatile on purpose!  parsers are not expected to cross thread boundaries during a single step
  private[this] var lastDerivation: (Line, Parser[A]) = _
  private[this] var finishMemo: List[A] = _     // cannot memoize failure

  protected var nullableMemo: Nullable = Nullable.Maybe   // future optimization idea: Byte {-1, 0, 1} to shrink object map

  def map[B](f: A => B): Parser[B] = Parser.Reduce(this, { (_, a: A) => f(a) :: Nil })

  def map2[B, C](that: Parser[B])(f: (A, B) => C): Parser[C] = (this ~ that) map f.tupled

  def ~[B](that: Parser[B]): Parser[A ~ B] = Parser.Sequence(this, that)

  def ~>[B](that: Parser[B]): Parser[B] =
    (this ~ that) map { case _ ~ b => b }

  def <~[B](that: Parser[B]): Parser[A] =
    (this ~ that) map { case a ~ _ => a }

  def ^^^[B](b: B): Parser[B] = map { _ => b }

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
  def apply[F[+_]: Monad](ls: LineStream[F]): F[List[ParseError] \/ List[A]] = {
    ls.normalize flatMap {
      case LineStream.More(line, tail) =>
        val derivation = derive(line)

        val ls2: F[LineStream[F]] = line.next map { l =>
          Monad[F].pure(LineStream.More(l, tail))
        } getOrElse tail

        ls2 flatMap { derivation(_) }

      case LineStream.Empty() =>
        Monad[F].pure(finish(Set()))
    }
  }

  // graph rendering is complicated... :-/
  final def renderCompact: String = {
    def renderNonterminal(label: String, target: List[Render.TokenSequence]): State[RenderState, String] = {
      require(!target.isEmpty)

      val init = label :: "::=" :: Nil

      def handleSequence(seq: Render.TokenSequence): State[RenderState, List[String]] =
        for {
          st <- State.get[RenderState]
          (map, _) = st

          inverted = Map(map.toList map {
            case (label, (target, _)) => target -> label
          }: _*)

          rendered <- seq traverse {
            case -\/(p) =>
              if (inverted contains p) {
                State.pure[RenderState, List[String]](inverted(p) :: Nil)
              } else {
                p.render flatMap handleSequence
              }

            case \/-(str) =>
              State.pure[RenderState, List[String]](str :: Nil)
          }
        } yield rendered.flatten

      for {
        renderedBranches <- target traverse handleSequence
        rendered = renderedBranches reduce { _ ::: "|" :: _ }
      } yield (init ::: rendered) mkString " "
    }

    val renderAll = for {
      start <- render

      st <- State.get[RenderState]
      (nts, labels) = st

      // if we ARE a non-terminal, relabel as start
      relabeled <- nts find { case (_, (p, _)) => p eq this } traverse {
        case (label, (target, branches)) =>
          val nts2 = nts - label + ("𝑆" -> ((target, branches)))
          val labels2 = labels - label + "𝑆"

          State.set((nts2, labels2))
      }

      startRender <- if (relabeled.isDefined)
        State.pure[RenderState, Option[String]](None)
      else
        renderNonterminal("𝑆", start :: Nil) map { Some(_) }

      // shadow the earlier state
      st <- State.get[RenderState]
      (nts, _) = st

      allRendered <- nts.toList traverse {
        case (label, (_, branches)) =>
          for {
            tokenSequences <- branches traverse { _.render }
            rendered <- renderNonterminal(label, tokenSequences)
          } yield rendered
      }
    } yield (startRender.toList ::: allRendered) mkString " ; "

    renderAll runA ((Map(), Set())) value
  }

  protected type RenderState = (Map[String, (Parser[_], List[Parser[_]])], Set[String])
  protected def render: State[RenderState, Render.TokenSequence]

  protected def gatherBranches(root: Option[Parser[_]]): List[Parser[_]] = this :: Nil

  protected final def assignLabel(labels: Set[String]): (Set[String], String) = {
    val candidate = PossibleLabels(util.Random.nextInt(PossibleLabels.length))

    if (labels contains candidate)
      assignLabel(labels)
    else
      (labels + candidate, candidate)
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

    trace(s"DERIVE $this with '${line.head}'")

    val snapshot = lastDerivation

    if (snapshot != null && snapshot._1 == line) {
      trace(s"DERIVE CACHED: $snapshot")
      snapshot._2
    } else {
      val back = _derive(line)
      lastDerivation = line -> back

      trace(s"DERIVE(of $this) completed: $lastDerivation")

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

  sealed trait Terminal[+A] extends Parser[A]
  sealed trait Nonterminal[+A] extends Parser[A]

  final case class Sequence[+A, +B](left: Parser[A], right: Parser[B]) extends Nonterminal[A ~ B] {

    protected def _derive(line: Line): Parser[A ~ B] = {
      trace(s"deriving sequence ($this)")
      trace(s"   left.isNullable = ${left.isNullable}")

      if (left.isNullable) {
        lazy val nonNulled = left.derive(line) ~ right
        lazy val nulled = Reduce(right.derive(line), { (_, b: B) =>
          left.finish(Set()).toList.flatten map { (_, b) }
        })

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

    protected def render: State[RenderState, Render.TokenSequence] =
      State pure (-\/(left) :: -\/(right) :: Nil)
  }

  final case class Union[+A](_left: () => Parser[A], _right: () => Parser[A]) extends Nonterminal[A] {
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

    protected def render: State[RenderState, Render.TokenSequence] = {
      for {
        st <- State.get[RenderState]
        (nts, labels) = st

        back <- if (nts.values exists { case (p, _) => p eq this }) {
          State.pure[RenderState, Render.TokenSequence](-\/(this) :: Nil)
        } else {
          val (labels2, label) = assignLabel(labels)

          val branches = gatherBranches(None)

          for {
            _ <- State.set((nts + (label -> ((this, branches))), labels2))
          } yield -\/(this) :: Nil
        }
      } yield back
    }

    protected override def gatherBranches(root: Option[Parser[_]]): List[Parser[_]] = root match {
      case Some(p) if p eq this => this :: Nil
      case Some(_) =>
        left.gatherBranches(root) ::: right.gatherBranches(root)

      case None =>
        left.gatherBranches(Some(this)) ::: right.gatherBranches(Some(this))
    }
  }

  final case class Reduce[A, +B](target: Parser[A], f: (List[Line], A) => List[B]) extends Nonterminal[B] {

    protected def _derive(line: Line): Parser[B] =
      Reduce(target derive line, f)

    protected def _finish(seen: Set[Parser[_]]) =
      target finish seen map { rs => rs flatMap { f(Nil, _) } }   // TODO line accumulation used here!

    protected def render: State[RenderState, Render.TokenSequence] =
      State pure (-\/(target) :: \/-("↪") :: \/-("λ") :: Nil)
  }

  final case class Literal(literal: String, offset: Int = 0) extends Terminal[String] {
    require(literal.length > 0)
    require(offset < literal.length)

    nullableMemo = Nullable.False

    protected def _derive(line: Line): Parser[String] = {
      if (literal.charAt(offset) == line.head) {
        if (offset == literal.length - 1)
          Epsilon(literal)
        else
          Literal(literal, offset + 1)
      } else {
        Failure(ParseError.UnexpectedCharacter(line))
      }
    }

    protected def _finish(seen: Set[Parser[_]]) =
      -\/(ParseError.UnexpectedEOF :: Nil)

    protected def render: State[RenderState, Render.TokenSequence] =
      State pure ((s"'${literal substring offset}'" :: Nil) map { \/-(_) })
  }

  final case class Epsilon[+A](value: A) extends Terminal[A] {
    nullableMemo = Nullable.True

    override def ^^^[B](b: B): Parser[B] = Epsilon(b)

    protected def _derive(line: Line): Parser[A] =
      Failure(ParseError.UnexpectedTrailingCharacters(line))

    protected def _finish(seen: Set[Parser[_]]) = \/-(value :: Nil)

    protected def render: State[RenderState, Render.TokenSequence] =
      State pure ((s"ε=${value.toString}" :: Nil) map { \/-(_) })
  }

  final case class Failure(error: ParseError) extends Terminal[Nothing] {
    nullableMemo = Nullable.True

    protected def _derive(line: Line): Parser[Nothing] = this

    protected def _finish(seen: Set[Parser[_]]) = -\/(error :: Nil)

    protected def render: State[RenderState, Render.TokenSequence] =
      State pure (("!!" :: Nil) map { \/-(_) })
  }
}

private[parseback] sealed trait Render extends Product with Serializable

private[parseback] object Render {
  type TokenSequence = List[Parser[_] \/ String]

  final case class Nonterminal(label: String, branches: List[Parser[_]]) extends Render
  final case class Tokens(contents: TokenSequence) extends Render
}
