scalaVersion := "3.3.3"

lazy val root = (project in file(".")).settings(
  name := "Scala3-Examples"
)

ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-feature",
  "-Xcheck-macros",
  "-Ycheck:all", // also for checking macros
  "-Ycheck-mods",
  "-Ydebug-type-error",
  "-Xprint-types", // Without this flag, we will not see error messages for exceptions during given-macro expansion!
  "-Yshow-print-errors",
  "-language:experimental.macros",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:namedTypeArguments",
  "-language:dynamics",
  "-Ykind-projector:underscores",
  "-unchecked",
  "-no-indent"
)
version := "1.0"

// by default sbt run runs the program in the same JVM as sbt
//in order to run the program in a different JVM, we add the following
fork in run := true

//scalacOptions += "-target:17" // ensures the Scala compiler generates bytecode optimized for the Java 17 virtual machine

//We can also set the soruce and target compatibility for the Java compiler by configuring the JavaOptions in build.sbt

// javaOptions ++= Seq(
//   "-source",
//   "17",
//   "target",
//   "17"
// )
ThisBuild / semanticdbEnabled := true
ThisBuild / usePipelining     := true

// https://mvnrepository.com/artifact/com.github.plokhotnyuk.jsoniter-scala/jsoniter-scala-core
libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.33.2"

// https://mvnrepository.com/artifact/com.github.plokhotnyuk.jsoniter-scala/jsoniter-scala-macros
libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.33.2" % "provided"