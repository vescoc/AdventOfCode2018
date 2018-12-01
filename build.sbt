name := "AdventOfCode2018"

scalaVersion := "2.12.7"
scalacOptions ++= Seq("-deprecation", "-feature")

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
