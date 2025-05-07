val scala3Version = "3.6.4"

// Common settings for all projects
lazy val commonSettings = Seq(
  organization := "io.github.functionaljustin", 
  version := "0.5.0-SNAPSHOT", 
  scalaVersion := scala3Version
)

// Publishing settings separated from common settings
lazy val publishSettings = Seq(
  publishTo := {
    val owner = "functionaljustin"
    val repo = "duct"
    val nexus = "https://maven.pkg.github.com/" + owner
    if (isSnapshot.value) { 
      Some("github-snapshots" at nexus + "/" + repo)
    } else { 
      Some("github-releases" at nexus + "/" + repo)
    }
  },
  credentials += Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "functionaljustin",
    System.getenv("GITHUB_TOKEN")
  )
)

// The 'core' library project
lazy val core = project
  .settings(
    commonSettings,
    name := "duct",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M11" % Test
  )
  // Only apply publish settings when explicitly publishing
  .settings(Seq(
    publish / credentials := {
      if (sys.env.contains("CI_PUBLISH") || sys.env.contains("GITHUB_TOKEN")) {
        (publish / credentials).value
      } else {
        Seq.empty
      }
    },
    publishLocal / credentials := {
      if (sys.env.contains("CI_PUBLISH") || sys.env.contains("GITHUB_TOKEN")) {
        (publishLocal / credentials).value
      } else {
        Seq.empty
      }
    }
  ) ++ (if (sys.env.contains("CI_PUBLISH")) publishSettings else Seq.empty))

// The 'examples' project, depends on 'core'
lazy val examples = project
  .dependsOn(core)
  .settings(
    commonSettings,
    name := "examples",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0-M11" % Test
    ),
    publish / skip := true
  )

// Root project
lazy val root = (project in file("."))
  .aggregate(core, examples)
  .settings(
    commonSettings,
    name := "duct-root",
    publish / skip := true
  )
