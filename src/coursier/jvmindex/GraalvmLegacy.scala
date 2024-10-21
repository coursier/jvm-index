package coursier.jvmindex

import Index.Os

object GraalvmLegacy {

  private def ghOrg  = "graalvm"
  private def ghProj = "graalvm-ce-builds"

  private def javaVersions = Seq("8", "11", "16", "17", "19")

  def fullIndex(ghToken: String): Index = {

    val releases = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)
      .map(_.tagName)
      .toVector

    val indices = javaVersions.map(v => index(releases, ghToken, v))

    indices.foldLeft(Index.empty)(_ + _)
  }

  def index(
    releases: Seq[String],
    ghToken: String,
    javaVersion: String
  ): Index = {

    val javaVersionInName0 = javaVersion != "8"
    val name =
      if (javaVersionInName0)
        s"jdk@graalvm-java$javaVersion"
      else
        "jdk@graalvm"

    val assetNamePrefix = s"graalvm-ce-java$javaVersion-"

    def osOpt(input: String): Option[(Os, String)] =
      if (input.startsWith("linux-"))
        Some((Os("linux"), input.stripPrefix("linux-")))
      else if (input.startsWith("darwin-"))
        Some((Os("darwin"), input.stripPrefix("darwin-")))
      else if (input.startsWith("windows-"))
        Some((Os("windows"), input.stripPrefix("windows-")))
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

    val indices = releases
      .filter(_.startsWith("vm-"))
      .flatMap { tagName =>
        val version = tagName.stripPrefix("vm-")
        val assets  = Asset.releaseAssets(ghOrg, ghProj, ghToken, tagName)
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
