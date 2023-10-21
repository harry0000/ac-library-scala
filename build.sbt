lazy val supportedScalaVersions = List("3.3.1", "3.3.0")

lazy val root = project
  .in(file("."))
  .settings(
    organization := "io.github.acl4s",
    name := "ac-library-scala",
    version := "0.1.0-SNAPSHOT",
    description := "A Scala port of AtCoder Library (ACL).",
    licenses := List(License.CC0),
    crossScalaVersions := supportedScalaVersions,
    scalaVersion := supportedScalaVersions.head,
    scalacOptions ++= List(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
