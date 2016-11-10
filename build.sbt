name := "evemail-smtp"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += Resolver.jcenterRepo


libraryDependencies ++= Seq(
  "eveapi" %% "esi-client" % "0.4.0",
  "com.lihaoyi" %% "fastparse" % "0.4.1"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
