scalaVersion := "2.13.3"
organization := "test"
name := "swetest"
version := "0.0.1-SNAPSHOT"


val CatsVer = "2.3.0"
val scalatestVer = "3.2.11"
val jmhVer = "1.34"
val jmhAnnotVer = "1.9.3"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalatestVer % "test",
                            "org.typelevel" %% "cats-core" % CatsVer,
                            "org.openjdk.jmh" % "jmh-core" % jmhVer ,
                            "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhAnnotVer)
//https://stackoverflow.com/questions/63051751/upgrading-to-scala-2-13-giving-scalaoptions-error
scalacOptions --= Seq(
  "-Xlint:by-name-right-associative", //SIP-34
  "-Xlint:nullary-override",
  "-Xlint:unsound-match",
  "-Yno-adapted-args"
)
//enablePlugins(JmhPlugin)