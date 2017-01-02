package parseback

sealed trait ParseError extends Product with Serializable

object ParseError {
  final case class UnexpectedTrailingCharacters(loc: Line) extends ParseError
  final case class UnexpectedCharacter(loc: Line) extends ParseError
}
