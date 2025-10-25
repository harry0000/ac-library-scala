lazy val supportedScalaVersions = List("3.7.2", "3.3.0")

lazy val commonSettings = Seq(
  organization := "io.github.acl4s",
  version := "0.1.0-SNAPSHOT",
  licenses := List(License.CC0),
  crossScalaVersions := supportedScalaVersions,
  scalaVersion := supportedScalaVersions.head,
  scalacOptions ++= List(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:all"
  ),
  libraryDependencies += "org.scalameta" %% "munit" % "1.2.0" % Test,
  Test / parallelExecution := false
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "ac-library-scala",
    description := "A Scala port of AtCoder Library (ACL)."
  )

lazy val example = project
  .in(file("example"))
  .dependsOn(root)
  .settings(commonSettings)
  .settings(
    name := "ac-library-scala-example",
    description := "Example codes using ac-library-scala.",
    Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary,
    publish / skip := true
  )
