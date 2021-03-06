object Settings {
  val project       = "script-engine"

  val company       = "scommon"

  val organization  = "org.scommon"

  val scalaVersion  = "2.10.2"

  val scalacOptions = Seq("-deprecation", "-unchecked", "-feature", "-Xelide-below", "900")
  val javacOptions  = Seq("-Xlint:unchecked")

  def prompt        = GitPrompt.build
}

