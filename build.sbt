name := "dynamiteSSA"

version := "1.0"

//scalaVersion := "2.12.1"
scalaVersion := "2.11.8"

// https://mvnrepository.com/artifact/org.clojure/clojure
libraryDependencies += "org.clojure" % "clojure" % "1.8.0"

// https://mvnrepository.com/artifact/ch.epfl.lamp/dotty-compiler_2.11
libraryDependencies += "ch.epfl.lamp" % "dotty-compiler_2.11" % "0.1.1-20170109-be64643-NIGHTLY"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
