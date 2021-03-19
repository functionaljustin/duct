lazy val root = project
  .in(file("."))
  .settings(
    name := "eval-example",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC1",
    useScala3doc := true,

    scalacOptions ++= Seq(
        //"-print-tasty",
        //"-explain", */
        //"-explain-types" */
    ),
    
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test"
    ))

