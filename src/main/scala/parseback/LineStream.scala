package parseback

import cats.{Eval, Now}

// a head-tail stream of [[Line]]s, with the tail computed in effect F[_]
sealed trait LineStream[F[+_]]

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
