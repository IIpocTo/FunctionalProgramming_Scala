name := "mephi_FP"
version := "1.0"
scalaVersion := "2.11.8"

resolvers ++= Seq(Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.bintrayRepo("scalaz", "releases"),
    Resolver.bintrayRepo("megamsys", "scala"))

libraryDependencies += "io.megam" %% "newman" % "1.3.12"
libraryDependencies += "com.norbitltd" % "spoiwo_2.11" % "1.1.1"
libraryDependencies += "org.apache.poi" % "ooxml-schemas" % "1.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"