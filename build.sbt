val scala3Version = "3.6.4"

// Common settings for all projects
lazy val commonSettings = Seq(
  organization := "io.github.functionaljustin", // Changed to io.github convention
  version := "0.5.0-SNAPSHOT", // Or your desired version
  scalaVersion := scala3Version,
  // Optional: Specify where to publish.
  // This configuration is for GitHub Packages.
  // It relies on environment variables that are typically available in GitHub Actions.
  // functionaljustin should be your GitHub username or organization (e.g., "functionaljustin")
  // GITHUB_REPOSITORY should be the name of your GitHub repository (e.g., "duct")
  publishTo := {
    val owner = "functionaljustin"
    val repo = "duct"
    val nexus = "https://maven.pkg.github.com/" + owner
    if (isSnapshot.value) { // For SNAPSHOT versions
      Some("github-snapshots" at nexus + "/" + repo)
    } else { // For release versions
      Some("github-releases" at nexus + "/" + repo)
    }
  },
  // Credentials to publish to GitHub Packages.
  // It's best practice to use environment variables for the token.
  // GITHUB_TOKEN should be a Personal Access Token with `write:packages` scope.
  credentials += Credentials(
    "GitHub Package Registry",
    "maven.pkg.github.com",
    "functionaljustin",
    System.getenv("GITHUB_TOKEN")             // Your GitHub PAT
  )
)

// The 'core' library project - this is what you'll likely publish
lazy val core = project
  .settings(
    commonSettings, // Apply common settings, including publishing configuration
    name := "duct", // This will be the artifactId
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M11" % Test // Updated MUnit to a more recent M-release for Scala 3
    // No need to repeat publishTo or credentials here if they are in commonSettings
    // and ThisBuild / setting is not used for them in commonSettings.
    // If commonSettings used ThisBuild / organization, etc., then it would apply globally.
    // Since it's just `organization := ...`, it applies to projects that include commonSettings.
  )

// The 'examples' project, depends on 'core'
lazy val examples = project
  .dependsOn(core)
  .settings(
    commonSettings, // Apply common settings
    name := "examples",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0-M11" % Test // Updated MUnit
    ),
    // Crucially, skip publishing for the 'examples' project
    publish / skip := true
  )

// Optional: Define a root project to aggregate others if you run commands from the root
lazy val root = (project in file("."))
  .aggregate(core, examples)
  .settings(
    commonSettings, // Apply common settings if you want them for the root too (e.g. version)
    name := "duct-root",
    publish / skip := true // Typically, you don't publish the root aggregate project
  )
