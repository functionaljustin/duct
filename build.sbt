val scala3Version = "3.1.2"

lazy val commonSettings = Seq(
    organization := "org.justinhj",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    githubOwner := "justinhj",
    githubRepository := "duct"
    )

lazy val core = project
  .settings(
    commonSettings,
    name := "duct",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M5" % Test
  )

lazy val examples = project
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "examples",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M5" % Test
  )
