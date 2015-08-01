scalaVersion := "2.11.6"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.12.4" % "test"
libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.52"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"
