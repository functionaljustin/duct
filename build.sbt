val scala3Version = "3.3.0"

lazy val commonSettings = Seq(
    organization := "org.justinhj",
    version := "0.3.0-SNAPSHOT",
    scalaVersion := scala3Version
    )

lazy val core = project
  .settings(
    commonSettings,
    name := "duct",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M6" % Test
  )

lazy val examples = project
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "examples",
    libraryDependencies ++= Seq(
      // apache commons
      "commons-io" % "commons-io" % "2.11.0",
      "org.scalameta" %% "munit" % "1.0.0-M6" % Test
    )
  )
