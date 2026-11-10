package coursier.jvmindex

import Index.{Arch, Os}

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

    def archOpt(input: String): Option[(Arch, String)] =
      if (input.startsWith("x64_"))
        Some((Arch("amd64"), input.stripPrefix("x64_bin")))
      else if (input.startsWith("aarch64_"))
        Some((Arch("arm64"), input.stripPrefix("aarch64_bin")))
      else
        None

    def archiveTypeOpt(input: String): Option[String] =
      if (input == "zip") Some("zip")
      else if (input == "tar.gz") Some("tgz")
      else None

    def entries(
      tagName: String,
      version: String,
      assetNameRegex: scala.util.matching.Regex
    ): Seq[Index] = {
      val assets       = Asset.releaseAssets(ghOrg, ghProj, ghToken, tagName).toVector
      val assetsByName = assets.map(asset => asset.name -> asset).toMap
      assets.flatMap { asset =>
        val nameGlobal  = "jdk@graalvm-community"
        val nameVersion = s"jdk@graalvm-java$javaVersion"
        val opt         =
          for {
            case assetNameRegex(name0) <- Some(asset.name)
            (os, rem)    <- osOpt(name0)
            (arch, rem0) <- archOpt(rem)
            ext          <- Some(rem0)
              .filter(_.startsWith("."))
              .map(_.stripPrefix("."))
            archiveType <- archiveTypeOpt(ext)
            entry = Asset.indexEntry(asset, archiveType, assetsByName)
          } yield Seq(
            Index(os, arch, nameGlobal, version, entry),
            Index(os, arch, nameVersion, version, entry)
          )
        opt.toSeq.flatten
      }
    }

    // Regular releases, like tag jdk-25.0.2, with assets like
    // graalvm-community-jdk-25.0.2_linux-x64_bin.tar.gz
    // Indexed with the JDK version (25.0.2 here)
    val regularIndices = releases
      .filter(_.startsWith(s"jdk-$javaVersion"))
      .flatMap { tagName =>
        val version = tagName.stripPrefix("jdk-")
        entries(
          tagName,
          version,
          (java.util.regex.Pattern.quote(s"graalvm-community-jdk-${version}_") + "(.*)").r
        )
      }

    // "Innovation" releases, like tag graal-25.5.5.0.3, with assets like
    // graalvm-community-jdk-25i5-25.0.5.0.3_linux-x64_bin.tar.gz
    // Indexed with the Graal version (25.5.5.0.3 here)
    val innovationTagRegex = s"graal-($javaVersion\\.([1-9][0-9]*)\\..*)".r
    val innovationIndices  = releases
      .collect {
        case tagName @ innovationTagRegex(version, innovation) =>
          (tagName, version, innovation)
      }
      .flatMap {
        case (tagName, version, innovation) =>
          entries(
            tagName,
            version,
            (java.util.regex.Pattern.quote(
              s"graalvm-community-jdk-${javaVersion}i$innovation-"
            ) + "[0-9.]+_(.*)").r
          )
      }

    val indices = regularIndices ++ innovationIndices

    indices.foldLeft(Index.empty)(_ + _)
  }

}
