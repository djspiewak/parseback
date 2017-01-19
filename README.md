# parseback [![Build Status](https://travis-ci.org/djspiewak/parseback.svg?branch=master)](https://travis-ci.org/djspiewak/parseback) [![Gitter](https://badges.gitter.im/djspiewak/parseback.svg)](https://gitter.im/djspiewak/parseback?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

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

## Usage

```sbt
resolvers += "bintray-djspiewak-maven" at "http://dl.bintray.com/djspiewak/maven"

val ParsebackVersion = "0.1"

libraryDependencies += "com.codecommit" %% "parseback-core" % ParsebackVersion

libraryDependencies += "com.codecommit" %% "parseback-cats" % ParsebackVersion
// or!
libraryDependencies += "com.codecommit" %% "parseback-scalaz-72" % ParsebackVersion
```

The current, "stable" version of parseback is 0.1.  Cross builds are available for Scala 2.12, 2.11 and 2.10.  I say "stable" in scare-quotes because this should obviously be considered fairly experimental software.  It's just barely stable enough that I'm willing to call it an "0.x" release.

All stable, numbered releases are signed with [my key](https://keybase.io/djspiewak).

Snapshots are intermittently available.  All snapshots are of the form `[version]-[hash]`, where `[version]` is the *compatible* base version (i.e. the stable version with which the snapshot is compatible).  If that base version is unreleased (i.e. a future release), then full compatibility with the ultimate release is not necessarily guaranteed.  `[hash]` is a seven character prefix of the Git hash from which the snapshot release was made.  Feel free to [make snapshots of your own!](#forks-and-releases).

Note that the `-cats`/`-scalaz-72` artifacts were not required prior to snapshot `0.2-caf7787`.  This snapshot was compiled and published against [shims](https://github.com/djspiewak/shims) version `0.4.1-cf0a86b`.

## Motivation

There are two important things that need to be motivated:

- Generalized parsing
- Parseback as an implementation thereof

If you don't believe that generalized parsing (i.e. a parsing algorithm which can handle the complete space of formal context-free grammars) is necessary, then parseback is a considerably over-complicated solution.  It still provides some nice features (many of which are a consequence of generalized parsing!), but there are simpler and faster algorithms which can achieve the goal.  That is, if the goal is parsing something less than the full space of context-free grammars.

### Generalized Parsing

Context-Free Grammars have several very nice properties as a formalism.  Notably, they:

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

The above is a BNF grammar which matches the string  any number of *balanced* bracket or parentheses operators, with the odd twist that brackets may close parentheses.  For example: `(((((((((((])))])]])))` is a valid string in the language defined by this grammar.

Now, you probably noticed the immediate redundancy across two of the reductions of the `E` non-terminal.  It seems intuitive that we should be able to extract out that redundancy into a separate, shared non-terminal.  Always DRY, right?

```
T ::= '(' E

E := T ')'
   | T ']'
   | epsilon
```

Unfortunately, this is no longer valid according to some parsing algorithms!  We have made a straightforward refactoring to the grammar which, according to the rules of CFG construction, should *not* have affected the language we match.  However, in some algorithms, it would have.

This is not an issue for generalized parsers such as parseback.

So generalized CFGs, as opposed to the limited variants supported by most parsing algorithms, give us a parsing analogue of referential transparency, and they give us closure under composition.  Again, closure under composition means that we can *always* compose to grammars, either via union or sequence, and the result is a grammar.  Put another way: with a generalized parsing algorithm, all combinators are *total*; with a non-generalized parsing algorithm, composition is *partial* and may fail on certain values.

### Why Parseback?

So if generalized parsing is so great, why not use one of the more mature implementations?  Well, for starters, there aren't too many truly mature generalized parsing implementations.  BISON has a "GLR mode", which is depressing and weird because it [doesn't actually implement GLR](https://lists.gnu.org/archive/html/help-bison/2004-10/msg00057.html).  There are a few well-polished research tools, notably [SGLR](http://www.meta-environment.org), which is rather inapproachable for non-research use.

To my knowledge (which is far from exhaustive!), there is only one other parser combinator implementation of a cubic time (current best-known bounds) generalized parsing algorithm in any language, and that is [gll-combinators](https://github.com/djspiewak/gll-combinators).  gll-combinators is pretty neat, and it's seen production use on several projects over the years, but it has some significant warts.  Namely, the algorithm is enormously more complex than PWD (Parsing With Derivatives), which makes maintenance and bug fixing much much harder in practice.  Second, the constant-time performance factors are very high.  They could be brought down considerably with more optimization work, but private correspondence with other research groups working on GLL suggests that the algorithm itself may simply be inherently less efficient than other generalized parsing algorithms due to the state which has to be threaded through the main dispatch loop.

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

### Why not PEG / Packrat / Parser Combinators?

Since the terms are not particularly well known, let's take a second to agree on what "PEG" and "Packrat" are.

Parsing Expression Grammars, or PEG, is a declarative format, analogous to BNF, which defines a grammar-like construct that will be accepted by a recursive-descent style parsing algorithm with infinite backtracking.  It consists of two primary composition operators: sequence, and *ordered choice*.  It does not contain a union operation, though ordered choice is somewhat reminiscent of union and may be used to encode similar grammatical semantics.

PEGs as a construction and as a parsing algorithm have gained considerable traction over the last decade, due in part to the subjectively intuitive properties of the ordered choice operation for disambiguation (it "tries" the first branch first), and also because of the ease with which the underlying algorithm may be implemented.  The parsing algorithm itself is exponential in the worst case (a recursively ambiguous grammar applied to input which it will *reject*), but is generally polynomial (with a moderately high exponent) on most inputs.  It does not admit left-recursive grammars, and "ambiguous" grammars will produce only a single result (due to the ordered choice).

Packrat parsing improves on PEG parsing through memoization, using a set of techniques very similar to those which are employed within PWD.  Packrat parsers are specified by PEGs, with the added benefit of admitting most left-recursive grammars, but not grammars with *indirect* left-recursion.  Packrat parsing is quadratic in the worst case, and generally somewhere between linear and quadratic on most inputs.  The memoization is quite tricky to implement, but nothing worse than PWD.

Parser combinators are familiar to anyone doing functional programming, and they literally implement PEG parsing.  The progression of parser combinator frameworks has more or less mirrored and borrowed-from the progression of PEG parsers, and most widely-used practical parser combinator frameworks are now implemented using some variant of Packrat parsing.  This includes Scala's standard library parser combinators.

Anyway, if these tools are so popular, why not use them?  My answer is simply that PEG parsing, as a fundamental tool, is broken.  Ordered choice seems very intuitive and pleasing when you first use it, but it becomes progressively harder and harder to encode expected semantics into a grammar.  A very common example of this is encoding *equal* precedence for the `+` and `-` binary operators in an expression grammar.  PEG parsing has no really good way of doing this.  And as your grammar gets more complicated and your disambiguation semantics become progressively more situational, you end up contorting your PEG expressions quite considerably just to get the ordering you need.  Your PEG "grammar" starts acting a lot more like an imperative program with slightly odd syntax, rather than a truly declarative abstraction.

One of parseback's fundamental guiding principles is that everything about parser construction should be declarative.  The grammar should specify the language precisely, and without imperative flow.  Disambiguation rules and negation classes should be similarly declarative, and uncluttered from the formal semantics of the CFG itself as they are, in fact, a function of tree construction.  Semantic actions (reductions) should be pure functions without a need for separate state tracking.  All of these things have been found to be enormously valuable properties in practice (based on experience with tools like SGLR and gll-combinators).  Parseback's goal is to bring these properties to Scala together with high performance and a clean, idiomatic API.

## Performance

About that performance...

Until we measure things, this is all just educated guesswork, loosely informed by the Adams, Hollenbeck, Might paper.  And until we finish laying down the fundamental algorithmic work, performance measurements are disingenuous.  However, it's not hard to prognosticate what is likely realistic, given what is known about the PWD algorithm, its implementations, and the JVM in general.

PWD is a worst-case cubic time algorithm.  That means its worst-case asymptotic performance is an order of magnitude *worse* than Packrat parsing (worst-case quadratic).  However, that worst-case appears to be quite infrequent in practice.  If you're accustomed to analyzing generalized parsing algorithms, it is pedagogically interesting to note that this worst case is *not* an out-growth of recursively ambiguous unions, but rather recursively *interleaved* alternation of sequentials.  PWD appears to be somewhat unique in that regard.  At any rate, it's not very common sort of construction in practice.

In practice, PWD's algorithmic complexity should be roughly linear, with some parse trails producing quadratic behavior on subsets of the input.  Experiments with various PWD implementations have shown that the inherent constant factors are *very* low.  Adams, Hollenbeck and Might found that the performance of their Racket implementation was within a few factors of BISON on raw C.  Even *approaching* that kind of performance is quite impressive, given the overhead of Racket's runtime.

Given that Parseback is being implemented on the JVM, I would expect the performance can be made to be very good.  I don't expect to beat BISON, but I do expect we'll be able to see roughly the same tier of performance that the Racket implementation achieved.  Or at the very least, I see absolutely no reason why we *shouldn't* see that tier of performance.  The algorithm is very straightforward.  The biggest concern is the number of allocations required, which is generally something the JVM doesn't handle as well as other runtimes.

*Assuming* we can achieve the performance that I believe is a realistic ceiling for this algorithm and API on the JVM, parseback should be pretty dramatically faster than Scala's standard library parser combinators.  I expect that parseback will remain slower than some heavily-optimized and specialized parsing frameworks such as Parboiled, but such frameworks are designed for a very different use-case (i.e. not language construction).  Pre-compiled tools like ANTLR and (especially!) Beaver will remain the gold standard for parser performance on the JVM, but my goal is to bring parseback's runtime performance close *enough* to these tools that it becomes a real, viable alternative, but (obviously) within the form of an embedded DSL with a pleasant API.

## Examples

*TODO*

## Forks and Releases

Fork away!  It's Github.  Parseback uses a hash-based version scheme for snapshot releases (see the comments in `build.sbt` for more details), which means that you should **feel free to publish snapshot releases** to your personal bintray (or other repositories).  The hash-based version prevents Ivy eviction from causing downstream conflicts, and also ensures that any other people pushing that same release will be producing identical artifacts.

I have two requests.  First, please *sign* your releases (with the `publishSigned` SBT command).  All releases that I make will be signed with my public key ([3587 7FB3 2BAE 5960](https://keybase.io/djspiewak)).  Furthermore, any stable release that I make will be tagged and the tagging commit will be signed with the same key.  Second, please *only* push snapshot releases (with versions ending in a git hash or `-SNAPSHOT`).  If multiple people push incompatible `0.3`s to different hosts, madness and confusion will ensue in downstream projects.  If you *need* to push a stable release (non-hash/snapshot), please change the group id.  That should prevent conflicts while also ensuring that no one is stuck waiting for me to publish a release that fixes their particular bug.

## Contributing and Legal

Contributions are very welcome!  Issuing a pull request constitutes your agreement to license your contributions under the Apache License 2.0 (see `LICENSE.txt`).  I will not incorporate changes without a pull request.  Please don't pull request someone else's work (unless they are explicitly involved in the PR on github).  If you create a new file, please be sure to replicate the copyright header from the other files.
