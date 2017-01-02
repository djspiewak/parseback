package parseback

import org.specs2.matcher.Matcher
import org.specs2.mutable._
import org.specs2.specification.SpecificationFeatures

trait ParsebackSpec extends Spec with SpecificationFeatures {

  final def parseOk[A](input: String)(results: A*): Matcher[Parser[A]] = { p: Parser[A] =>
    val maybeResults = p(LineStream(input)).value

    maybeResults match {
      case \/-(actual) =>
        val test: Boolean = actual must containTheSameElementsAs(results)

        (test, s"accepted '$input' with results $actual", s"accepted '$input' but produced results $actual, expected $results")

      case -\/(err) =>
        (false, "", s"failed to parse '$input' with error $err")
    }
  }

  final def failToParse(input: String)(error: ParseError): Matcher[Parser[_]] = { p: Parser[_] =>
    val maybeResults = p(LineStream(input)).value

    maybeResults match {
      case \/-(results) =>
        (false, "", s"failed to reject '$input' with results $results")

      case -\/(actual) =>
        (actual == error, s"rejected '$input' with error $actual", s"rejected '$input' with error $actual, expected $error")
    }
  }
}
