package coursier.jvmindex

object GhToken {

  lazy val token = Option(System.getenv("GH_TOKEN")).getOrElse {
    System.err.println(
      "Warning: GH_TOKEN not set, it's likely we'll get rate-limited by the GitHub API"
    )
    ""
  }

}
