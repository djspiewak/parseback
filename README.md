# parseback [![Build Status](https://travis-ci.org/djspiewak/parseback.svg?branch=master)](https://travis-ci.org/djspiewak/parseback) [![Gitter](https://img.shields.io/gitter/room/djspiewak/parseback.svg)](https://gitter.im/djspiewak/parseback) [![Bintray](https://img.shields.io/bintray/v/djspiewak/maven/parseback-core.svg)](https://bintray.com/djspiewak/maven/parseback-core)

> Parsing with derivatives may be viewed as a form of breadth-first
> recursive descent.  Breadth-first search is orthogonal to depth-first
> search, and two lines which are orthogonal in two-dimensional
> Euclidean space may also be said to be perpendicular.  Thus, an
> appropriate name for this library might be "parsper", as a shortening
> of "parse perpendicular".  Such a name is eminently googleable and
> implies a wealth of potential Star Trek puns.  Unfortunately, it looks
> far too much like a typo.  Thus, "parseback" serves both as a faux
> shortening of "parse backwards" and a reference to "logback", for no
> particular reason.

Parseback is a Scala implementation of [parsing with derivatives](http://matt.might.net/papers/might2011derivatives.pdf) (Might, Darais, Spiewak; 2011) taking into account the significant refinements of described by [Adams, Hollenbeck and Might](http://dl.acm.org/citation.cfm?id=2908128).  It is designed to be an idiomatic, convenient and safe parser combinator library for formal general context-free grammars with production-viable performance.

1. [Usage](#usage)
2. [Motivation](#motivation)
    1. [Generalized Parsing](#generalized-parsing)
    2. [Why Parseback?](#why-parseback)
    3. [Why Not PEG / Packrat / Parser Combinators?](#why-not-peg--packrat--parser-combinators)
3. [Performance](#performance)
4. [Example](#example)
    1. [Direct Grammar](#direct-grammar)
    2. [Application](#application)
    3. [Building Trees](#building-trees)
5. [DSL Reference](#dsl-reference)
6. [Forks and Releases](#forks-and-releases)
7. [Contributing and Legal](#contributing-and-legal)

## Usage

```sbt
resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"

val ParsebackVersion = "0.2.1"

libraryDependencies += "com.codecommit" %% "parseback-core" % ParsebackVersion

libraryDependencies += "com.codecommit" %% "parseback-cats" % ParsebackVersion
// or!
libraryDependencies += "com.codecommit" %% "parseback-scalaz-72" % ParsebackVersion
// or!
libraryDependencies += "com.codecommit" %% "parseback-scalaz-71" % ParsebackVersion
```

The current, "stable" version of parseback is **0.2.1**.  Cross builds are available for Scala 2.12 and 2.11.  I say "stable" in scare-quotes because this should obviously be considered fairly experimental software.  It's just barely stable enough that I'm willing to call it an "0.x" release.

All stable, numbered releases are signed with [my key](https://keybase.io/djspiewak).

Snapshots are intermittently available.  All snapshots are of the form `[version]-[hash]`, where `[version]` is the *compatible* base version (i.e. the stable version with which the snapshot is compatible).  If that base version is unreleased (i.e. a future release), then full compatibility with the ultimate release is not necessarily guaranteed.  `[hash]` is a seven character prefix of the Git hash from which the snapshot release was made.  Feel free to [make snapshots of your own!](#forks-and-releases).

Note that the Scalaz 7.1 artifact is only available in snapshots *after* hash `03b3d90`.

## Motivation

There are two important things that need to be motivated:

- Generalized parsing
- Parseback as an implementation thereof

If you don't believe that generalized parsing (i.e. a parsing algorithm which can handle the complete space of formal context-free grammars) is necessary, then parseback is a considerably over-complicated solution.  It still provides some nice features (many of which are a consequence of generalized parsing!), but there are simpler and faster algorithms which can achieve the goal.  That is, if the goal is parsing something less than the full space of context-free grammars.

### Generalized Parsing

Context-Free Grammars, or CFGs, have several very nice properties as a formalism.  Notably, they:

- Are closed under *union*
- Are closed under *sequence*
- Maintain identity under identity transformations

All of these points deserve closer examination.

Closure under *union* and *sequence* literally says that if you have two CFGs, `G` and `H`, then `G | H` and `G ~ H` are both also CFGs (using parseback notation).  That is, the union of `G` and `H` is a CFG (where `G` and `H` are CFGs), and the sequentialization (*this* followed by *that*) of `G` and `H` is also a CFG.  We take this property so much for granted when it comes to regular expressions (where it also holds) that it doesn't seem particularly revelatory, but most parsing algorithms do not allow for this.

Most parsing algorithms do not accept the full set of context-free grammars.  That is, there is a set of CFGs – in the case of some algorithms, a very large and useful set! – which will either be rejected by the parsing algorithm, or which will produce invalid results.  Invalid results may range from accepting/rejecting inputs erroneously, to simply crashing on certain inputs.  The subsets of CFGs which are accepted vary from algorithm to algorithm, but some common ones include:

- CNF
- LL(1)
- LL(k)
- LALR(1)
- PEG (not actually a subset of CFG; see below)

Factoring a grammar to fit into the particular rule set accepted by the parsing algorithm at hand can be very non-trivial.  Mechanical transformations are possible in many cases, but tooling is seldom available which implement these theoretical algorithms.  More often than not, we're stuck reading a BISON "shift/reduce conflict" error, wondering what juggling we need to perform in order to bring things up to par.

Identity transformations are the third property which is lost when general CFGs are not accepted by a parsing algorithm.  And this is, quite simply, the parsing analogue of referential transparency.  For example:

```
E := '(' E ')'
   | '(' E ']'
   | epsilon
```

The above is a BNF (Backus–Naur form) grammar which matches the string  any number of *balanced* bracket or parentheses operators, with the odd twist that brackets may close parentheses.  For example: `(((((((((((])))])]])))` is a valid string in the language defined by this grammar.

Now, you probably noticed the immediate redundancy across two of the reductions of the `E` non-terminal.  It seems intuitive that we should be able to extract out that redundancy into a separate, shared non-terminal.  Always DRY, right?

```
T ::= '(' E

E := T ')'
   | T ']'
   | epsilon
```

Unfortunately, this is no longer valid according to some parsing algorithms!  We have made a straightforward refactoring to the grammar which, according to the rules of CFG construction, should *not* have affected the language we match.  However, in some algorithms, it would have.

This is not an issue for generalized parsers such as parseback.

So generalized CFGs, as opposed to the limited variants supported by most parsing algorithms, give us a parsing analogue of referential transparency, and they give us closure under composition.  Again, closure under composition means that we can *always* compose two grammars, either via union or sequence, and the result is a grammar.  Put another way: with a generalized parsing algorithm, all combinators are *total*; with a non-generalized parsing algorithm, composition is *partial* and may fail on certain values.

### Why Parseback?

So if generalized parsing is so great, why not use one of the more mature implementations?  Well, for starters, there aren't too many truly mature generalized parsing implementations.  BISON has a "GLR mode", which is depressing and weird because it [doesn't actually implement GLR](https://lists.gnu.org/archive/html/help-bison/2004-10/msg00057.html).  There are a few well-polished research tools, notably [SGLR](http://www.meta-environment.org), which is rather inapproachable for non-research use.

To my knowledge (which is far from exhaustive!), there are only two other parser combinator implementations of a cubic time (current best-known bounds) generalized parsing algorithm in any language, and those are [gll-combinators](https://github.com/djspiewak/gll-combinators) and [Early](https://hackage.haskell.org/package/Earley).  Earley is a Haskell library, which rules it out if you're on the JVM ([for now](http://eta-lang.org)).  gll-combinators is pretty neat, and it's seen production use on several projects over the years, but it has some significant warts.  Namely, the algorithm is enormously more complex than PWD (Parsing With Derivatives), which makes maintenance and bug fixing much much harder in practice.  Second, the constant-time performance factors are very high.  They could be brought down considerably with more optimization work, but private correspondence with other research groups working on GLL suggests that the algorithm itself may simply be inherently less efficient than other generalized parsing algorithms due to the state which has to be threaded through the main dispatch loop.

Outside of gll-combinators and research tools like SGLR, there really isn't much that implements generalized parsing.  So if you're sold on the idea of *total* composability and referential transparency, parseback is probably your best bet.

Fortunately, we don't have to *only* point to practical-monopoly as a reason to use parseback.  Other useful features that are implemented or planned to be implemented:

- Scannerless (in practice, scannerless parsing all-but requires generalized CFGs due to eagerness)
- Line tracking (basically required if you're implementing a compiler)
- Disambiguation filters (in the style of SGLR, for declarative precedence/associativity)
- Negation classes (also from SGLR, used for identifiers as distinct from keywords)
- Error recovery (in the style of BISON; open research here)
- Incremental, resource-safe input consumption threaded through an arbitrary monad
- Clean Scala syntax and type inference

Ever wanted to pass an arity-3 lambda as a reduction rule for a Scala parser combinator instance of the form `a ~ b ~ c` and get full type inference?  Scala's parser combinators cannot do this.  Parseback can.

### Why Not PEG / Packrat / Parser Combinators?

Since the terms are not particularly well known, let's take a second to agree on what "PEG" and "Packrat" are.

Parsing Expression Grammars, or PEGs, is a declarative format, analogous to BNF, which defines a grammar-like construct that will be accepted by a recursive-descent style parsing algorithm with infinite backtracking.  It consists of two primary composition operators: sequence, and *ordered choice*.  It does not contain a union operation, though ordered choice is somewhat reminiscent of union and may be used to encode similar grammatical semantics.

PEGs as a construction and as a parsing algorithm have gained considerable traction over the last decade, due in part to the subjectively intuitive properties of the ordered choice operation for disambiguation (it "tries" the first branch first), and also because of the ease with which the underlying algorithm may be implemented.  The parsing algorithm itself is exponential in the worst case (a recursively ambiguous grammar applied to input which it will *reject*), but is generally polynomial (with a moderately high exponent) on most inputs.  It does not admit left-recursive grammars, and "ambiguous" grammars will produce only a single result (due to the ordered choice).

Packrat parsing improves on PEG parsing through memoization, using a set of techniques very similar to those which are employed within PWD.  Packrat parsers are specified by PEGs, with the added benefit of admitting most left-recursive grammars, but not grammars with *indirect* left-recursion.  Packrat parsing is quadratic in the worst case, and generally somewhere between linear and quadratic on most inputs.  The memoization is quite tricky to implement, but nothing worse than PWD.

Parser combinators are familiar to anyone doing functional programming, and they literally implement PEG parsing.  The progression of parser combinator frameworks has more or less mirrored and borrowed-from the progression of PEG parsers, and most widely-used practical parser combinator frameworks are now implemented using some variant of Packrat parsing.  This includes Scala's standard library parser combinators.

So, if these tools are so popular, why not use them?  My answer is simply that PEG parsing, as a fundamental tool, is broken.  Ordered choice seems very intuitive and pleasing when you first use it, but it becomes progressively harder and harder to encode expected semantics into a grammar.  A very common example of this is encoding *equal* precedence for the `+` and `-` binary operators in an expression grammar.  PEG parsing has no really good way of doing this.  And as your grammar gets more complicated and your disambiguation semantics become progressively more situational, you end up contorting your PEG expressions quite considerably just to get the ordering you need.  Your PEG "grammar" starts acting a lot more like an imperative program with slightly odd syntax, rather than a truly declarative abstraction.

One of parseback's fundamental guiding principles is that everything about parser construction should be declarative.  The grammar should specify the language precisely, and without imperative flow.  Disambiguation rules and negation classes should be similarly declarative, and uncluttered from the formal semantics of the CFG itself as they are, in fact, a function of tree construction.  Semantic actions (reductions) should be pure functions without a need for separate state tracking.  All of these things have been found to be enormously valuable properties in practice (based on experience with tools like SGLR and gll-combinators).  Parseback's goal is to bring these properties to Scala together with high performance and a clean, idiomatic API.

## Performance

About that performance...

Until we measure things, this is all just educated guesswork, loosely informed by the Adams, Hollenbeck, Might paper.  And until we finish laying down the fundamental algorithmic work, performance measurements are disingenuous.  However, it's not hard to prognosticate what is likely realistic, given what is known about the PWD algorithm, its implementations, and the JVM in general.

PWD is a worst-case cubic time algorithm.  That means its worst-case asymptotic performance is an order of magnitude *worse* than Packrat parsing (worst-case quadratic).  However, that worst-case appears to be quite infrequent in practice.  If you're accustomed to analyzing generalized parsing algorithms, it is pedagogically interesting to note that this worst case is *not* an out-growth of recursively ambiguous unions, but rather recursively *interleaved* alternation of sequentials.  PWD appears to be somewhat unique in that regard.  At any rate, it's not very common sort of construction in practice.

In practice, PWD's algorithmic complexity should be roughly linear, with some parse trails producing quadratic behavior on subsets of the input.  Experiments with various PWD implementations have shown that the inherent constant factors are *very* low.  Adams, Hollenbeck and Might found that the performance of their Racket implementation was within a few factors of BISON on raw C.  Even *approaching* that kind of performance is quite impressive, given the overhead of Racket's runtime.

Given that Parseback is being implemented on the JVM, I would expect the performance can be made to be very good.  I don't expect to beat BISON, but I do expect we'll be able to see roughly the same tier of performance that the Racket implementation achieved.  Or at the very least, I see absolutely no reason why we *shouldn't* see that tier of performance.  The algorithm is very straightforward.  The biggest concern is the number of allocations required, which is generally something the JVM doesn't handle as well as other runtimes.

*Assuming* we can achieve the performance that I believe is a realistic ceiling for this algorithm and API on the JVM, parseback should be pretty dramatically faster than Scala's standard library parser combinators.  I expect that parseback will remain slower than some heavily-optimized and specialized parsing frameworks such as Parboiled, but such frameworks are designed for a very different use-case (i.e. not language construction).  Pre-compiled tools like ANTLR and (especially!) Beaver will remain the gold standard for parser performance on the JVM, but my goal is to bring parseback's runtime performance close *enough* to these tools that it becomes a real, viable alternative, but (obviously) within the form of an embedded DSL with a pleasant API.

## Example

This section will assume the following imports:

```scala
import parseback._
import parseback.compat.cats._
```

If you're using scalaz, you should replace `cats` with `scalaz`, as well as make other alterations below to the cats-specific types (e.g. replace `Eval` with `Need`).

A time-honored example of parsers is the expression grammar, and who are we to argue with tradition!  The following parser reads and evaluates simple arithmetic expressions over integers:

### Direct Grammar

```scala
implicit val W = Whitespace("""\s+"""r)

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
  | "-" ~ expr  ^^ { (_, _, e) => -e }
  | """\d+""".r ^^ { (_, str) => str.toInt }
)
```

The first thing that you should notice is that this looks *very* similar to the [expression grammar example](https://en.wikipedia.org/wiki/Context-free_grammar#Algebraic_expressions) given on Wikipedia.  In fact, the grammar is identical save for three features:

- Negation
- Integer literals
- Precedence and associativity

The Wikipedia article on CFGs actually specifically calls out the precedence and associativity question, where as we are presciently side-stepping it for the time being.  As for negation and integers, it just seemed like a more interesting example if I included them.

At any rate, this should give a general feel for how parseback's DSL works that should be familiar to anyone who has used a parser generator tool (such as BISON) in the past.  Values of type `Parser` represent grammar fragments – generally non-terminals – and can be composed either via the `~` operator (sequentially) or via the `|` operator (alternation).  It is idiomatic to vertically align definitions by the `|` operator, so as to mimic the conventional structure of a BNF grammar.  Remember, this is a *declarative* definition of the underlying grammar.

The first bit of weirdness you should notice as a user of Scala is that all of these parsers are declared as `lazy val`.  This is extremely important.  The Scala compiler would not complain if we used `def` here, but the algorithm wouldn't provide the same guarantees.  In fact, if you use `def` instead of `lazy val`, the PWD algorithm becomes exponential instead of cubic!  This is because `def` defeats memoization by creating an infinite lazy tree.  This is different from `lazy val`, which represents the cyclic graph directly.  Packrat parsing frameworks have a similar restriction.

There are a few more symbols to unpack here before we move on to how we actually *use* this tool.  First, the mysterious `^^` operator which apparently takes a lambda as its parameter.  This operator is effectively the `map` operation on `Parser`.  Note that `Parser` does define a `map` which obeys the functor laws and which has a slightly different signature.  Specifically, it does not provide line tracking (a feature we'll cover in a bit).  Symbolic operators, and especially the `^^` operator, have better precedence interactions with the rest of the DSL and are generally quite lightweight, which is why we're using `^^` here instead of an English name in the DSL.

The *meaning* of the `^^` operator is the same as `map` though: take the `Parser` to the left and "unpack" it, applying the given function to its result and return a new `Parser` which will produce the returned value.  In standard parser generator terms, `^^` denotes a semantic action, or "reduction".  The action itself is given by the lambda which is passed to `^^`.  This lambda will take the following parameters:

1. The line(s) which were parsed to produce the results
2. The first result in a sequential composition
3. The second result in a sequential composition
4. *etc…*

Thus, if you apply `^^` to a `Parser` created from a single rule (such as the very last rule in `factor`), its lambda will take *two* parameters: one for the line, and one for the result.  If you apply `^^` to a `Parser` created from the composition of two rules (such as the negation rule in `factor`), it will take *three* parameters: one for the line, and one for each of the results.  If you apply `^^` to a `Parser` created from the composition of three rules (such as any of the actions in `expr` or `term`), it will take *four* parameters: one for the line, and one for each of the results.  In the cases of the `expr` and `term` reductions, as well as the negation rule in `factor`, we don't care about the results from the syntax operators (e.g. `"*"`, `"-"`, and so on), since these are trivial.  What we care about are the results from the `expr`, `term` and `factor` parsers.  These are captured in the relevant function parameters, and then used in the computation.

Sharp-eyed readers will realize that the following grammar production is a bit different than the others: `"(" ~> expr <~ ")"`.  Specifically, this is using the `~>` and `<~` operators.  These operators behave exactly the same as the `~` operator – sequential composition of two parsers – except they only preserve the result in the direction of the "arrow".  Parentheses are a mixfix operator which only matters for grouping purposes and does not affect the result of the expression.  Thus, the results obtained by computing the value of an expression wrapped in parentheses is simply the value of the expression within the parentheses, unmodified.  We could have equivalently written this production in the following way:

```scala
"(" ~> expr <~ ")"
// is equivalent to!
"(" ~ expr ~ ")" ^^ { (_, _, e, _) => e }
```

Obviously, the former is quite a bit more concise than the latter.  You should decide on a case-by-case basis whether or not it is worth using the `~>` or `<~` operators in your grammars.  For example, you'll notice I didn't use them in the negation operation, despite the fact that the result of the `"-"` parser is meaningless.  Whether or not you use the "arrow" operators rather than plain-old sequential composition (`~`) is up to you, but I will say from experience that *over*-use of these operators can make it very difficult to read a grammar.  Sometimes, the simpler tools are best.

One final note: that mysterious `implicit val W = Whitespace(...)` thing that we put at the very top.  This is clearly not part of the grammar.  What we're actually doing here is configuring the behavior of the scannerless tokenizer within parseback.  Parseback does implement scannerless parsing, as you probably inferred from the "bare" regular expression within the `factor` non-terminal, but doing so requires a bit of configuration in order to avoid assuming things about your language.  Specifically, you must tell parseback exactly what characters count as whitespace and thus can be ignored leading and trailing tokens.  This is important, since some languages have significant whitespace!  By default, parseback will not assume any whitespace semantics at all, and all characters will be parsed.  We override this default by providing an implicit value of type `Whitespace`.  This value must be in scope both at the grammar declaration site *and* at the point where we apply the grammar to an input (see below).

Note that parseback's whitespace handling is currently extremely naive.  The only whitespace regular expressions which will behave appropriately are of the form `.+`, where `.` is "any single character class".  Thus, `\s+` is valid, as is `[ \t]+`, but `//[^\n]*|\s+` is not.  We hope to lift this restriction soon, but it requires some work on the algorithm.

### Application

So how do we apply a parser to a given input stream, now that we've declared the parser?  The first thing we need to do is convert the input stream into a `LineStream`.  `LineStream[F]` is an abstraction inside parseback representing a lazy cons list of lines, where each tail is produced within an effect, `F`.  It is very literally `StreamT[F, Line]`, where `Line` is a `String` with some metadata (notably including line number).  This is a relatively broad abstraction, and allows for basically anything which can be incrementally produced within a monadic effect, including `fs2.Stream` (or to be more precise, `fs2.Pull`), `IO`, `Future`, or in the case of our simple examples, `Eval`.

`LineStream` contains some helpful constructors which can build streams from some common inputs, including a plain-old `String` (which may contain multiple lines).  This is the constructor we're going to use in all of our examples:

```scala
import cats.Eval

val input: LineStream[Eval] = LineStream[Eval]("1 + 2")
```

The exact monad doesn't really matter here, so long as it obeys the monad laws.  We're constructing a line stream, `input`, from an already-in-memory value, so there is no need for any sort of complex effect control.  In fact, the constructor `String => LineStream[F]` accepts any `Applicative[F]`, and `cats.Eval` is just a convenient one.  Another convenient applicative might be `scalaz.Need`.

Once we have our `input`, we can apply our parser to it to produce a result:

```scala
val result: Eval[Either[List[ParseError], List[Int]]] = expr(input)
```

Note that the `apply` method on `Parser` requires a `Monad[F]`, where the `F` comes from the `LineStream[F]` it was supplied.  Naturally, we have to get the result "out" of the monad in order to see it, which is trivial in the case of `Eval`:

```scala
result.value      // => Right(List(3))
```

The results are contained within an `Either`, and then a `List` within that.  The left side of the `Either` is how errors can be represented in the case of invalid input, while the right side is where the parsed values are.  As this is a generalized parser, it is possible (when you have a globally ambiguous grammar) to produce more than one result for a single input.  For example, this might have happened to us if we hadn't factored our grammar to encode precedence.

And there you have it!  We can apply our parser to any input we like.  For example, we can test that precedence is working correctly:

```scala
expr(LineStream[Eval]("3 + 4 * 5")).value       // => Right(List(23))
expr(LineStream[Eval]("(3 + 4) * 5")).value     // => Right(List(35))
```

If we hadn't correctly encoded precedence by factoring the grammar (specifically, by splitting `expr` and `term`), the first line above would have produced `Right(List(23, 35))`, reflecting the fact that *both* interpretations would be permissible.

And just as a sanity check, we can verify that things which shouldn't parse, don't:

```scala
expr(LineStream[Eval]("3 + *")).value
```

This will produce the following value (within the `Left(List(`):

```scala
UnexpectedCharacter(Line("3 + *", 0, 4), Set("""\d+""", "-", "("))
```

The `Line` here contains line number and column offset information indicating the `*` character, while the `Set` indicates the possible *expected* valid tokens which *could* have appeared at that location.  Thus, even though PWD isn't a predictive parsing algorithm, it is still able to give fairly decent error messages.  Pretty-printed, this error might look like this:

```
error:1:5: unexpected character, '*', expected one of: \d+, -, (
  3 + *
      ^
```

So long as you can construct a `LineStream[F]` for something, and that `F` has a lawful `Monad`, you can parse it.  For example, here's a simple function that applies a parser to an [fs2](https://github.com/functional-streams-for-scala/fs2) `Stream` of lines:

```scala
import fs2._

def parseLines[F[_], A](lines: Stream[F, String], parser: Parser[A]): Stream[F, Either[ParseError, A]] = {
  def mkLines(h: Handle[F, String]): Pull[F, Nothing, LineStream[Pull[F, Nothing, ?]]] =
    h.await1Option map {
      case Some((line, h2)) => LineStream.More(Line(line), mkLines(h2))
      case None             => LineStream.Empty()
    }

  lines.pull { h =>
    mkLines(h) flatMap { ls => parser(ls) } flatMap {
      case Left(errs)     => Pull output (Chunk seq (errs map { Right(_) }))
      case Right(results) => Pull output (Chunk seq (results map { Left(_) }))
    }
  }
}
```

And there you have generalized, incremental parsing on an ephemeral stream, effectively for free.  We're taking advantage of the fact that fs2 allows us to incrementally traverse a `Stream` via `Pull`, which forms a monad in its resource.

### Building Trees

Evaluating expressions in-place is all well and good, but parseback's API is really designed for building ASTs for use in larger systems, such as compilers.  In order to do this, we're going to need some structure:

```scala
sealed trait Expr {
  def loc: List[Line]
}

final case class Mul(loc: List[Line], left: Expr, right: Expr) extends Expr
final case class Div(loc: List[Line], left: Expr, right: Expr) extends Expr
final case class Add(loc: List[Line], left: Expr, right: Expr) extends Expr
final case class Sub(loc: List[Line], left: Expr, right: Expr) extends Expr
final case class Neg(loc: List[Line], inner: Expr) extends Expr
final case class Num(loc: List[Line], value: Int) extends Expr
```

This is a very straightforward AST that includes line information, just as you might expect inside a simple compiler.  We can modify our original grammar to produce this AST, rather than directly evaluating the expressions:

```scala
implicit val W = Whitespace("""\s+"""r)

lazy val expr: Parser[Expr] = (
    expr ~ "+" ~ term ^^ { (loc, e, _, t) => Add(loc, e, t) }
  | expr ~ "-" ~ term ^^ { (loc, e, _, t) => Sub(loc, e, t) }
  | term
)

lazy val term: Parser[Expr] = (
    expr ~ "*" ~ factor ^^ { (loc, e, _, f) => Mul(loc, e, t) }
  | expr ~ "/" ~ factor ^^ { (loc, e, _, f) => Div(loc, e, t) }
  | factor
)

lazy val factor: Parser[Expr] = (
    "(" ~> expr <~ ")"
  | "-" ~ expr  ^^ { (loc, _, e) => Neg(loc, e) }
  | """\d+""".r ^^ { (loc, str) => Num(loc, str.toInt) }
)
```

Now if we apply our parser to a sample input, we should receive an expression tree, rather than a value:

```scala
expr(LineStream[Eval]("2 * -3 + 4")).value
```

Within the "either of lists", this will produce:

```scala
Mul(_, Num(_, 2), Add(_, Neg(_, Num(_, 3)), Num(_, 4)))
```

I've elided the `List[Line]` values, just to make things more clear, but suffice it to say that they are there, and they represent the line(s) on which the given construct may be found.  This information is very commonly baked into an AST at parse time so that it can be used later in compilation or interpretation to produce error messages relevant to the original declaration site in syntax.  Each AST node contains *all* lines which comprise its syntactic elements, not just the first one, so you have maximal flexibility in how you want to represent things.  If all you want is the first line, you're certainly free to call `.head` on the `loc` value inside of the parser reductions (the list of lines is always guaranteed to be non-empty).

## DSL Reference

- `|` – Composition of two parsers by union.  Both parsers must produce the same type
- `~` – Composition of two parsers by sequence.  Both results are retained in a `Tuple2` (parseback defines infix `~` as an alias for `Tuple2`)
  + `~>` – Composition of two parsers by sequence, discarding the left result
  + `<~` – Composition of two parsers by sequence, discarding the right result
- `^^` – Map the results of a parser by the specified function, which will be passed the lines consumed by the parser to produce its results
  + `^^^` – Map the results of a parser to a single constant value
- `"..."` – Construct a parser which consumes exactly the given string, producing that string as a result
- `"""...""".r` – Construct a parser which consumes any string which matches the given regular expression, producing the matching string as a result.  Regular expressions may not span multiple lines, and beginning-of-line/end-of-line may be matched with the `^`/`$` anchors.
- `()` – Construct a parser which always succeeds, consuming nothing, producing `()` as a result

## Forks and Releases

Fork away!  It's Github.  Parseback uses a hash-based version scheme for snapshot releases (see the comments in `build.sbt` for more details), which means that you should **feel free to publish snapshot releases** to your personal bintray (or other repositories).  The hash-based version prevents Ivy eviction from causing downstream conflicts, and also ensures that any other people pushing that same release will be producing identical artifacts.

I have two requests.  First, please *sign* your releases (with the `publishSigned` SBT command).  All releases that I make will be signed with my public key ([3587 7FB3 2BAE 5960](https://keybase.io/djspiewak)).  Furthermore, any stable release that I make will be tagged and the tagging commit will be signed with the same key.  Second, please *only* push snapshot releases (with versions ending in a git hash or `-SNAPSHOT`).  If multiple people push incompatible `0.3`s to different hosts, madness and confusion will ensue in downstream projects.  If you *need* to push a stable release (non-hash/snapshot), please change the group id.  That should prevent conflicts while also ensuring that no one is stuck waiting for me to publish a release that fixes their particular bug.

## Contributing and Legal

Contributions are very welcome!  Issuing a pull request constitutes your agreement to license your contributions under the Apache License 2.0 (see `LICENSE.txt`).  I will not incorporate changes without a pull request.  Please don't pull request someone else's work (unless they are explicitly involved in the PR on github).  If you create a new file, please be sure to replicate the copyright header from the other files.
