package parseback

import cats.{Eval, Monad, Now}
import cats.syntax.all._

// a head-tail stream of [[Line]]s, with the tail computed in effect F[_]
sealed trait LineStream[F[+_]] {
  import LineStream._

  /**
   * Eliminates *leading* empty lines.  Once a non-empty line is hit,
   * the remainder of the stream is ignored.  This is lazy within the
   * monad, and thus does not force IO.
   */
  def normalize(implicit F: Monad[F]): F[LineStream[F]] = this match {
    case More(line, tail) if line.isEmpty =>
      tail flatMap { _.normalize }

    case m @ More(_, _) => F pure m

    case Empty() => F pure Empty()
  }
}

object LineStream {

  def apply(str: String): LineStream[Eval] = {
    val splits = str split """\r|\r?\n"""
    val (front, last) = splits splitAt (splits.length - 1)

    apply((front map { _ + "\n" }) ++ last)
  }

  def apply(lines: Seq[String]): LineStream[Eval] = {
    val actuals = lines.zipWithIndex map {
      case (str, lineNo) => Line(str, lineNo, 0)
    }

    actuals.foldRight(Empty(): LineStream[Eval]) { (line, tail) => More(line, Now(tail)) }
  }

  final case class More[F[+_]](line: Line, tail: F[LineStream[F]]) extends LineStream[F]
  final case class Empty[F[+_]]() extends LineStream[F]
}
