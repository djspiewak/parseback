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

libraryDependencies += "com.codecommit" %% "parseback-core" % ParsebackVersion
```

At the present time, the only released versions are snapshots.  If you're feeling extremely brave (or foolish), you can try the following snapshot:

```sbt
val ParsebackVersion = "0.1-be2e240"
```

Cross builds are available for Scala 2.12, 2.11 and 2.10.

## Examples

*TODO*

## Forks and Releases

Fork away!  It's Github.  Parseback uses a hash-based version scheme for snapshot releases (see the comments in `build.sbt` for more details), which means that you should **feel free to publish snapshot releases** to your personal bintray (or other repositories).  The hash-based version prevents Ivy eviction from causing downstream conflicts, and also ensures that any other people pushing that same release will be producing identical artifacts.

I have two requests.  First, please *sign* your releases (with the `publishSigned` SBT command).  All releases that I make will be signed with my public key ([3587 7FB3 2BAE 5960](https://keybase.io/djspiewak)).  Furthermore, any stable release that I make will be tagged and the tagging commit will be signed with the same key.  Second, please *only* push snapshot releases (with versions ending in a git hash or `-SNAPSHOT`).  If multiple people push incompatible `0.3`s to different hosts, madness and confusion will ensue in downstream projects.  If you *need* to push a stable release (non-hash/snapshot), please change the group id.  That should prevent conflicts while also ensuring that no one is stuck waiting for me to publish a release that fixes their particular bug.

## Contributing and Legal

Contributions are very welcome!  Issuing a pull request constitutes your agreement to license your contributions under the Apache License 2.0 (see `LICENSE.txt`).  I will not incorporate changes without a pull request.  Please don't pull request someone else's work (unless they are explicitly involved in the PR on github).  If you create a new file, please be sure to replicate the copyright header from the other files.
