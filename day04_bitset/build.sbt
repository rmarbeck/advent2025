val scala3Version = "3.3.6"


lazy val adventday04_bitset = (project in file(".")).
  settings(
    name := "day04_bitset",
    version := "0.9",
    description := "Advent of code : day04_bitset",

    scalaVersion := scala3Version,

    scalacOptions := Seq("-unchecked", "-deprecation"),

    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
      "ch.qos.logback" % "logback-classic" % "1.5.19",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
      "org.scalameta" %% "munit" % "1.2.0" % Test
    )
  )
