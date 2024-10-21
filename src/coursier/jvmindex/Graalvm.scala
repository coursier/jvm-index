package coursier.jvmindex

import Index.Os

object Graalvm {

  private def ghOrg  = "graalvm"
  private def ghProj = "graalvm-ce-builds"

  def fullIndex(ghToken: String): Index = {

    val releases = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)
      .map(_.tagName)
      .toVector

    (Iterator(17) ++ Iterator.from(20))
      .map(v => index(releases, ghToken, v.toString))
      .takeWhile(!_.isEmpty)
      .foldLeft(Index.empty)(_ + _)
  }

  def index(
    releases: Seq[String],
    ghToken: String,
    javaVersion: String
  ): Index = {

    def osOpt(input: String): Option[(Os, String)] =
      if (input.startsWith("linux-"))
        Some((Os("linux"), input.stripPrefix("linux-")))
      else if (input.startsWith("macos-"))
        Some((Os("darwin"), input.stripPrefix("macos-")))
      else if (input.startsWith("windows-"))
        Some((Os("windows"), input.stripPrefix("windows-")))
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

    val indices = releases
      .filter(_.startsWith(s"jdk-$javaVersion"))
      .flatMap { tagName =>
        val version         = tagName.stripPrefix("jdk-")
        val assetNamePrefix = s"graalvm-community-jdk-${version}_"
        val assets          = Asset.releaseAssets(ghOrg, ghProj, ghToken, tagName)
        assets
          .filter(asset => asset.name.startsWith(assetNamePrefix))
          .flatMap { asset =>
            val name0       = asset.name.stripPrefix(assetNamePrefix)
            val nameGlobal  = "jdk@graalvm-community"
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
