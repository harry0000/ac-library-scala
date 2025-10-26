import scala.scalanative.build.Mode

lazy val supportedScalaVersions = List("3.7.2", "3.3.0")

lazy val commonSettings = Seq(
  organization := "io.github.acl4s",
  version := "0.2.0-SNAPSHOT",
  licenses := List(License.CC0),
  crossScalaVersions := supportedScalaVersions,
  scalaVersion := supportedScalaVersions.head,
  scalacOptions ++= List(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:all"
  ),
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.0" % Test,
  Test / parallelExecution := false
)

lazy val acl4s =
  crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("."))
    .settings(commonSettings)
    .settings(
      name := "ac-library-scala",
      description := "A Scala port of AtCoder Library (ACL)."
    )
    .nativeSettings(
      nativeConfig ~= { _.withMode(Mode.releaseFull) }
    )

lazy val acl4sJVM = acl4s.jvm
lazy val acl4sNative = acl4s.native

lazy val example =
  crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("example"))
    .dependsOn(acl4s)
    .settings(commonSettings)
    .settings(
      name := "ac-library-scala-example",
      description := "Example codes using ac-library-scala.",
      Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary,
      publish / skip := true
    )
    .nativeSettings(
      nativeConfig ~= { _.withMode(Mode.releaseFast) }
    )

lazy val exampleJVM = example.jvm
lazy val exampleNative = example.native
