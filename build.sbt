name := "mephi_FP"
version := "1.0"
scalaVersion := "2.12.0"

//Нужно раскомментить для работы lab0
/*resolvers ++= Seq(Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.bintrayRepo("scalaz", "releases"),
    Resolver.bintrayRepo("megamsys", "scala"))

libraryDependencies += "io.megam" %% "newman" % "1.3.12"*/

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"