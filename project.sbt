/*
 * Copyright 2017 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.sys.process._

addCommandAlias("ci", ";test ;mimaReportBinaryIssues")

organization in ThisBuild := "com.codecommit"

name := "parseback"

/*
 * Compatibility version.  Use this to declare what version with
 * which `master` remains in compatibility.  This is literally
 * backwards from how -SNAPSHOT versioning works, but it avoids
 * the need to pre-declare (before work is done) what kind of
 * compatibility properties the next version will have (i.e. major
 * or minor bump).
 *
 * As an example, the builds of a project might go something like
 * this:
 *
 * - 0.1-hash1
 * - 0.1-hash2
 * - 0.1-hash3
 * - 0.1
 * - 0.1-hash1
 * - 0.2-hash2
 * - 0.2
 * - 0.2-hash1
 * - 0.2-hash2
 * - 1.0-hash3
 * - 1.0-hash4
 * - 1.0
 *
 * The value of BaseVersion starts at 0.1 and remains there until
 * compatibility with the 0.1 line is lost, which happens just
 * prior to the release of 0.2.  Then the base version again remains
 * 0.2-compatible until that compatibility is broken, with the major
 * version bump of 1.0.  Again, this is all to avoid pre-committing
 * to a major/minor bump before the work is done (see: Scala 2.8).
 */
val BaseVersion = "0.4.0"

licenses in ThisBuild += ("Apache-2.0", url("http://www.apache.org/licenses/"))

bintrayVcsUrl in ThisBuild := Some("git@github.com:djspiewak/parseback.git")

publish := {}
publishLocal := {}
publishArtifact := false

val coursierSettings = Seq(
  coursierUseSbtCredentials := true,
  coursierChecksums := Nil      // workaround for nexus sync bugs
)

val bintraySettings = Seq(
  credentials in bintray := {
    val old = (credentials in bintray).value

    if (isTravisBuild.value)
      Nil
    else
      old
  }
)

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val TagBase = """^(\d+)\.(\d+).*"""r
    val TagBase(major, minor) = BaseVersion

    val tags = "git tag --list".!! split "\n" map { _.trim }

    val versions =
      tags filter { _ startsWith s"v$major.$minor" } map { _ substring 1 }

    versions map { v => organization.value %% name.value % v } toSet
  },

  resolvers ++= {
    if (isTravisBuild.value)
      Seq("bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven")
    else
      Seq()
  }
)

addCommandAlias("measure-all", "benchmarks/jmh:run -rff results.csv")
addCommandAlias("measure", "benchmarks/jmh:run -rff results.csv .*parsebackRun")
addCommandAlias("profile", "benchmarks/jmh:run -prof jmh.extras.JFR -f 1 .*parsebackRun")

lazy val root = project
  .in(file("."))
  .aggregate(benchmarks, coreJVM, coreJS)
  .settings(coursierSettings, bintraySettings)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .dependsOn(coreJVM)
  .settings(
    name := "parseback-benchmarks",
    coursierSettings,
    bintraySettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.codecommit"         %% "gll-combinators"          % "2.3",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"),

    publish := {},
    publishLocal := {},
    publishArtifact := false,

    sourceDirectory in Jmh := (sourceDirectory in Compile).value)
  .enablePlugins(JmhPlugin)

lazy val core = crossProject
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "parseback-core",
    coursierSettings,
    bintraySettings,
    mimaSettings)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-core" % "1.1.0",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck"        % "1.13.4"       % Test,
      "org.specs2"     %% "specs2-core"       % Versions.Specs % Test,
      "org.specs2"     %% "specs2-scalacheck" % Versions.Specs % Test
    ),
    initialCommands := "import parseback._",
    scalacOptions in Test += "-Yrangepos",
    logBuffered in Test := false,
    scalacOptions in (Compile, console) ~= (_ filterNot (Set(
      "-Xfatal-warnings",
      "-Ywarn-unused-import").contains)),
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

/***********************************************************************\
                      Boilerplate below these lines
\***********************************************************************/

// Adapted from Rob Norris' post at https://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code"
)

scalacOptions in ThisBuild ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 11 => Seq(
      "-Ywarn-unused-import", // Not available in 2.10
      "-Ywarn-numeric-widen" // In 2.10 this produces a some strange spurious error
    )
    case _ => Seq.empty
  }
}

scalacOptions in ThisBuild ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 12 || scalaVersion.value == "2.11.11" =>
      Seq("-Ypartial-unification")

    case _ => Seq.empty
  }
}

libraryDependencies in ThisBuild +=
  compilerPlugin("org.spire-math" % "kind-projector" % "0.9.6" cross CrossVersion.binary)

libraryDependencies in ThisBuild ++= {
  scalaVersion.value match {
    case "2.11.8" => Seq(compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full))
    case "2.10.6" => Seq(compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full))
    case _ => Seq.empty
  }
}

enablePlugins(GitVersioning)

val ReleaseTag = """^v([\d\.]+)$""".r

git.baseVersion := BaseVersion

git.gitTagToVersionNumber := {
  case ReleaseTag(version) => Some(version)
  case _ => None
}

git.formattedShaVersion := {
  val suffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, git.uncommittedSignifier.value)

  git.gitHeadCommit.value map { _.substring(0, 7) } map { sha =>
    git.baseVersion.value + "-" + sha + suffix
  }
}

// we need the following because jgit bugs out on symlinks
git.gitUncommittedChanges := "git status -s".!!.trim.length > 0
