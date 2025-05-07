val scala3Version = "3.6.4"

lazy val commonSettings = Seq(
    organization := "org.functionaljustin",
    version := "0.5.0-SNAPSHOT",
    scalaVersion := scala3Version
    )

lazy val core = project
  .settings(
    commonSettings,
    name := "duct",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M8" % Test
  )

lazy val examples = project
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "examples",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0-M8" % Test
    )
  )
