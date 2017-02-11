package parseback.benchmarks

import org.openjdk.jmh.annotations._

import scala.util.parsing.{combinator => spc}

@State(Scope.Thread)
@Fork(2)
@Measurement(iterations = 10)
@Warmup(iterations = 10)
@Threads(1)
class ArithmeticBenchmarks {

  @Param(Array("2", "8", "32", "128", "512", "2048"))
  var size: Int = _

  val parseback = {
    import _root_.parseback._

    lazy val expr: Parser[Int] = (
        expr ~ "+" ~ term ^^ { (_, e, _, t) => e + t }
      | expr ~ "-" ~ term ^^ { (_, e, _, t) => e - t }
      | term
    )

    lazy val term: Parser[Int] = (
        term ~ "*" ~ factor ^^ { (_, e, _, f) => e * f }
      | term ~ "/" ~ factor ^^ { (_, e, _, f) => e / f }
      | factor
    )

    lazy val factor: Parser[Int] = (
        "(" ~> expr <~ ")"
      | "-" ~ factor       ^^ { (_, _, e) => -e }
      | """\d+""".r        ^^ { (_, str) => str.toInt }
    )

    expr
  }

  val gll = {
    import com.codecommit.gll.RegexParsers._

    lazy val expr: Parser[Int] = (
        expr ~ "+" ~ term ^^ { (e, _, t) => e + t }
      | expr ~ "-" ~ term ^^ { (e, _, t) => e - t }
      | term
    )

    lazy val term: Parser[Int] = (
        term ~ "*" ~ factor ^^ { (e, _, f) => e * f }
      | term ~ "/" ~ factor ^^ { (e, _, f) => e / f }
      | factor
    )

    lazy val factor: Parser[Int] = (
        "(" ~> expr <~ ")"
      | "-" ~ factor       ^^ { (_, e) => -e }
      | """\d+""".r        ^^ { _.toInt }
    )

    expr
  }

  object ScalaParserCombinator extends spc.RegexParsers with spc.PackratParsers {

    lazy val expr: Parser[Int] = (
        expr ~ "+" ~ term ^^ { case e ~ _ ~ t => e + t }
      | expr ~ "-" ~ term ^^ { case e ~ _ ~ t => e - t }
      | term
    )

    lazy val term: Parser[Int] = (
        term ~ "*" ~ factor ^^ { case e ~ _ ~ f => e * f }
      | term ~ "/" ~ factor ^^ { case e ~ _ ~ f => e / f }
      | factor
    )

    lazy val factor: Parser[Int] = (
        "(" ~> expr <~ ")"
      | "-" ~ factor       ^^ { case _ ~ e => -e }
      | """\d+""".r        ^^ { _.toInt }
    )

    def run(input: String) = parse(expr, input)
  }

  def additionText: String =
    0 until size map { i => i.toString } mkString "+"

  @Benchmark
  def parsebackAddition(): Unit = {
    import _root_.parseback.LineStream
    import _root_.parseback.compat.cats._

    import cats.Eval

    val stream = LineStream[Eval](additionText)
    parseback(stream).value
  }

  @Benchmark
  def gllAddition(): Unit = {
    import com.codecommit.gll.LineStream

    val stream = LineStream(additionText)
    gll(stream)
  }

  @Benchmark
  def spcAddition(): Unit = {
    ScalaParserCombinator.run(additionText)
  }
}
