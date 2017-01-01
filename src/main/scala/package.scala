package object parseback {
  type ~[+A, +B] = (A, B)

  val ~ = Tuple2

  final implicit class ParserSyntax[A](self: => Parser[A]) {
    def |(that: => Parser[A]): Parser[A] = Parser.Union(() => self, () => that)
  }
}
