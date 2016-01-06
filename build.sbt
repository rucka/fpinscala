name := "fpinscala"

scalaVersion := "2.11.7"

libraryDependencies ++= {
    val ScalaTest   = "2.2.6"
    Seq(
        "org.scalatest" %% "scalatest" % ScalaTest % "test"
    )
}