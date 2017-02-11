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

addCommandAlias("measure-all", "jmh:run -rff results.csv")
addCommandAlias("measure", "jmh:run -rff results.csv .*parsebackRun")
addCommandAlias("profile", "jmh:run -prof jmh.extras.JFR -f 1 .*parsebackRun")

libraryDependencies ++= Seq(
  "com.codecommit"         %% "gll-combinators"          % "2.3",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5")

enablePlugins(JmhPlugin)

publish := ()
publishLocal := ()
publishArtifact := false

sourceDirectory in Jmh := (sourceDirectory in Compile).value
