scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Scala3-Examples"
  )
  .settings(
    scalacOptions ++= Seq(
      "-no-indent"
    )
  )

version := "1.0"

// by default sbt run runs the program in the same JVM as sbt
//in order to run the program in a different JVM, we add the following
fork in run := true

scalacOptions += "-target:17" // ensures the Scala compiler generates bytecode optimized for the Java 17 virtual machine

//We can also set the soruce and target compatibility for the Java compiler by configuring the JavaOptions in build.sbt

// javaOptions ++= Seq(
//   "-source",
//   "17",
//   "target",
//   "17"
// )
ThisBuild / semanticdbEnabled := true
