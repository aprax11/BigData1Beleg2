name := "Beleg2Aufgabe"
version := "0.1"

scalaVersion := "2.13.1"
run := Defaults.runTask(fullClasspath in Runtime, mainClass in run in Compile, runner in run).evaluated

libraryDependencies ++=Seq(
"org.scalactic" %% "scalactic" % "3.1.1",
			   "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  	"org.jfree" % "jfreechart" % "1.0.19")


