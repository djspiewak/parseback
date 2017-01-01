package parseback

// a head-tail stream of [[Line]]s, with the tail computed in effect F[_]
sealed trait LineStream[+F[+_]]

object LineStream {
  final case class More[F[+_]](line: Line, tail: F[LineStream[F]]) extends LineStream[F]
  case object Empty extends LineStream[Nothing]
}
