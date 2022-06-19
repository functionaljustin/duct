val scala3Version = "3.1.2"

lazy val core = project
  .settings(
    name := "duct",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

lazy val examples = project
  .dependsOn(core)
  .settings(
    name := "examples",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
