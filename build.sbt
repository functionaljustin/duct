lazy val duct = project
  .in(file("duct"))
  .settings(
    name := "duct",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC1",
    useScala3doc := true,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test"
    ))

lazy val ductExamples = project
  .in(file("examples"))
  .settings(
    name := "duct-examples",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC1",
    useScala3doc := true,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test"
    ))

