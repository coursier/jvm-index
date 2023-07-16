object GraalvmLegacy {

  def fullIndex(ghToken: String): Index = {
    val graalvmIndex0      = index(ghToken, "8")
    val graalvmJdk11Index0 = index(ghToken, "11")
    val graalvmJdk16Index0 = index(ghToken, "16")
    val graalvmJdk17Index0 = index(ghToken, "17")
    val graalvmJdk19Index0 = index(ghToken, "19")
    graalvmIndex0 + graalvmJdk11Index0 + graalvmJdk16Index0 + graalvmJdk17Index0 + graalvmJdk19Index0
  }

  def index(
    ghToken: String,
    javaVersion: String,
    javaVersionInName: java.lang.Boolean = null
  ): Index = {

    val javaVersionInName0 = Option(javaVersionInName)
      .map(x => x: Boolean)
      .getOrElse(javaVersion != "8")
    val name =
      if (javaVersionInName0)
        s"jdk@graalvm-java$javaVersion"
      else
        "jdk@graalvm"

    val ghOrg  = "graalvm"
    val ghProj = "graalvm-ce-builds"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    val assetNamePrefix = s"graalvm-ce-java$javaVersion-"

    def osOpt(input: String): Option[(String, String)] =
      if (input.startsWith("linux-"))
        Some(("linux", input.stripPrefix("linux-")))
      else if (input.startsWith("darwin-"))
        Some(("darwin", input.stripPrefix("darwin-")))
      else if (input.startsWith("windows-"))
        Some(("windows", input.stripPrefix("windows-")))
      else
        None

    def archOpt(input: String): Option[(String, String)] =
      if (input.startsWith("amd64-"))
        Some(("amd64", input.stripPrefix("amd64-")))
      else if (input.startsWith("aarch64-"))
        Some(("arm64", input.stripPrefix("aarch64-")))
      else
        None

    def archiveTypeOpt(input: String): Option[String] =
      if (input == "zip") Some("zip")
      else if (input == "tar.gz") Some("tgz")
      else None

    val indices = releases0
      .filter(release => release.tagName.startsWith("vm-"))
      .flatMap { release =>
        val version = release.tagName.stripPrefix("vm-")
        val assets  = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.tagName)
        assets
          .filter(asset => asset.name.startsWith(assetNamePrefix))
          .flatMap { asset =>
            val name0 = asset.name.stripPrefix(assetNamePrefix)
            val opt = for {
              (os, rem)    <- osOpt(name0)
              (arch, rem0) <- archOpt(rem)
              ext <- Some(rem0)
                .filter(_.startsWith(version + "."))
                .map(_.stripPrefix(version + "."))
              archiveType <- archiveTypeOpt(ext)
            } yield Index(os, arch, name, version, archiveType + "+" + asset.downloadUrl)
            opt.toSeq
          }
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}
