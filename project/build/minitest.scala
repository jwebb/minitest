// Minitest (c) 2011 Jamie Webb
// MIT License

import sbt._

class MiniTest(info : ProjectInfo) extends DefaultProject(info) {
	val junit = "junit" % "junit" % "4.8.2"
	val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test->default"
}

// vim:sw=4:ts=4:noet
