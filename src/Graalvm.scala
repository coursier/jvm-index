object Graalvm {

  def fullIndex(ghToken: String): Index = {
    val graalvmJdk17Index0 = index(ghToken, "17")
    val graalvmJdk20Index0 = index(ghToken, "20")
    val graalvmJdk21Index0 = index(ghToken, "21")
    graalvmJdk17Index0 + graalvmJdk20Index0 + graalvmJdk21Index0
  }

  def index(
    ghToken: String,
    javaVersion: String
  ): Index = {

    val ghOrg  = "graalvm"
    val ghProj = "graalvm-ce-builds"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    def osOpt(input: String): Option[(String, String)] =
      if (input.startsWith("linux-"))
        Some(("linux", input.stripPrefix("linux-")))
      else if (input.startsWith("macos-"))
        Some(("darwin", input.stripPrefix("macos-")))
      else if (input.startsWith("windows-"))
        Some(("windows", input.stripPrefix("windows-")))
      else
        None

    def archOpt(input: String): Option[(String, String)] =
      if (input.startsWith("x64_"))
        Some(("amd64", input.stripPrefix("x64_bin")))
      else if (input.startsWith("aarch64_"))
        Some(("arm64", input.stripPrefix("aarch64_bin")))
      else
        None

    def archiveTypeOpt(input: String): Option[String] =
      if (input == "zip") Some("zip")
      else if (input == "tar.gz") Some("tgz")
      else None

    val indices = releases0
      .filter(release => release.tagName.startsWith(s"jdk-$javaVersion"))
      .flatMap { release =>
        val version         = release.tagName.stripPrefix("jdk-")
        val assetNamePrefix = s"graalvm-community-jdk-${version}_"
        val assets          = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.tagName)
        assets
          .filter(asset => asset.name.startsWith(assetNamePrefix))
          .flatMap { asset =>
            val name0       = asset.name.stripPrefix(assetNamePrefix)
            val nameGlobal  = "jdk@graalvm"
            val nameVersion = s"jdk@graalvm-java$javaVersion"
            val opt =
              for {
                (os, rem)    <- osOpt(name0)
                (arch, rem0) <- archOpt(rem)
                ext <- Some(rem0)
                  .filter(_.startsWith("."))
                  .map(_.stripPrefix("."))
                archiveType <- archiveTypeOpt(ext)
              } yield Seq(
                Index(os, arch, nameGlobal, version, archiveType + "+" + asset.downloadUrl),
                Index(os, arch, nameVersion, version, archiveType + "+" + asset.downloadUrl)
              )
            opt.toSeq.flatten
          }
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}
