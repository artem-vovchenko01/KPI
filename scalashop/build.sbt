scalaVersion := "2.13.0"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter" % "0.21",
  "com.storm-enroute" %% "scalameter-core" % "0.21",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
  "com.novocode" % "junit-interface" % "0.11" % Test
)
libraryDependencies += "org.scalameta" %% "munit" % "0.7.27"  % Test
testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
