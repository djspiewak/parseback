package parseback

import util.EitherSyntax._

// semantics must mirror Nullable
sealed trait Results[+A] {
  import Results._

  final def map[B](f: A => B): Results[B] = this pmap { _ map f }

  final def pmap[B](f: List[A] => List[B]): Results[B] = this match {
    case Success(values) => Success(f(values))
    case Failure(errors) => Failure(errors)
    case Hypothetical(errors) => Hypothetical(errors)
  }

  // error merging is not commutative
  final def &&[B](that: Results[B]): Results[(A, B)] = (this, that) match {
    case (Success(v1), Success(v2)) => Success(v1 flatMap { a => v2 map { b => (a, b) } })
    case (Success(_), Failure(e)) => Failure(e)
    case (Failure(e), Success(_)) => Failure(e)
    case (Failure(e), Failure(_)) => Failure(e)
    case (Failure(e), Hypothetical(_)) => Failure(e)
    case (Hypothetical(h), Failure(_)) => Failure(h)
    case (Success(_), Hypothetical(h)) => Hypothetical(h)
    case (Hypothetical(h), Success(_)) => Hypothetical(h)
    case (Hypothetical(h1), Hypothetical(h2)) => Hypothetical(ParseError.prioritize(h1 ::: h2))
  }

  final def ||[B >: A](that: Results[B]): Results[B] = (this, that) match {
    case (Success(v1), Success(v2)) => Success(v1 ::: v2)
    case (Success(v), Failure(_)) => Success(v)
    case (Failure(_), Success(v)) => Success(v)
    case (Failure(e1), Failure(e2)) => Failure(ParseError.prioritize(e1 ::: e2))
    case (Failure(e), Hypothetical(h)) => Hypothetical(ParseError.prioritize(e ::: h))
    case (Hypothetical(h), Failure(e)) => Hypothetical(ParseError.prioritize(e ::: h))
    case (Success(v), Hypothetical(_)) => Success(v)
    case (Hypothetical(_), Success(v)) => Success(v)
    case (Hypothetical(h1), Hypothetical(h2)) => Hypothetical(ParseError.prioritize(h1 ::: h2))
  }

  final def toEither: List[ParseError] \/ List[A] = this match {
    case Success(results) => \/-(results)
    case Failure(errors) => -\/(errors)
    case Hypothetical(errors) => -\/(errors)
  }
}

object Results {
  sealed trait Cacheable[+A] extends Results[A]

  final case class Success[+A](values: List[A]) extends Cacheable[A]
  final case class Failure(errors: List[ParseError]) extends Cacheable[Nothing]
  final case class Hypothetical(errors: List[ParseError]) extends Results[Nothing]
}
