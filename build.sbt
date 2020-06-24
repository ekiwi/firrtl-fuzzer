name := "firrtl-fuzzer"
version := "0.1"
scalaVersion := "2.12.10"

scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11")

libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.3"

libraryDependencies += "com.pholser" % "junit-quickcheck-core" % "0.8"
libraryDependencies += "com.pholser" % "junit-quickcheck-generators" % "0.8"

// https://alvinalexander.com/scala/how-to-use-junit-testing-with-scala
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
