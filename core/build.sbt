libraryDependencies += "org.typelevel" %% "cats-core" % "0.8.+"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % Test

initialCommands := "import parseback._"

scalacOptions in Test += "-Yrangepos"

scalacOptions in (Compile, console) ~= (_ filterNot (Set("-Xfatal-warnings", "-Ywarn-unused-import").contains))

scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
