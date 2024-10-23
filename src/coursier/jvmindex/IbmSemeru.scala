package coursier.jvmindex

import Index.{Arch, Os}

object IbmSemeru {

  def fullIndex(ghToken: String): Index =
    (Iterator("8", "11") ++ Iterator.from(16).map(_.toString))
      .map(index(ghToken, _))
      .takeWhile(!_.isEmpty)
      .foldLeft(Index.empty)(_ + _)

  def index(
    ghToken: String,
    javaVersion: String
  ): Index = {

    val ghOrg  = "ibmruntimes"
    val ghProj = s"semeru$javaVersion-binaries"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    def osOpt(input: String): Option[(Os, String)] =
      input match
        case input if input.startsWith("linux") => Some((Os("linux"), input.stripPrefix("linux_")))
        case input if input.startsWith("mac")   => Some((Os("darwin"), input.stripPrefix("mac_")))
        case input if input.startsWith("windows") =>
          Some((Os("windows"), input.stripPrefix("windows_")))
        case input if input.startsWith("aix") => Some((Os("aix"), input.stripPrefix("aix_")))
        case _                                => None

    def archOpt(input: String): Option[(Arch, String)] =
      input match
        case input if input.startsWith("x64") => Some((Arch("amd64"), input.stripPrefix("x64_")))
        case input if input.startsWith("aarch64") =>
          Some((Arch("arm64"), input.stripPrefix("aarch64_")))
        case input if input.startsWith("ppc64le") =>
          Some((Arch("ppc64le"), input.stripPrefix("ppc64le_")))
        case input if input.startsWith("ppc64") =>
          Some((Arch("ppc64"), input.stripPrefix("ppc64_")))
        case input if input.startsWith("s390x") =>
          Some((Arch("s390x"), input.stripPrefix("s390x_")))
        case _ => None

    def archiveTypeOpt(input: String): Option[(String, String)] =
      input match
        case input if input.endsWith(".zip")    => Some(("zip", input.stripSuffix(".zip")))
        case input if input.endsWith(".tar.gz") => Some(("tgz", input.stripSuffix(".tar.gz")))
        case _                                  => None

    val indices = releases0
      .filter { release =>
        release.tagName.startsWith((if (javaVersion == "8") "jdk" else "jdk-") + javaVersion)
      }
      .flatMap { release =>
        val version =
          if (javaVersion == "8")
            release.tagName.stripPrefix("jdk").split("-b").apply(0).replace("8u", "8.0.")
          else
            release.tagName.stripPrefix("jdk-")
        val shortVersion    = version.takeWhile(_ != '+')
        val assetNamePrefix = "ibm-semeru-open-jdk_"
        val assets          = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.tagName)
        assets
          .filter(asset => asset.name.startsWith(assetNamePrefix))
          .flatMap { asset =>
            val assetName = asset.name.stripPrefix(assetNamePrefix)
            val name      = s"jdk@ibm-semeru-openj9-java$javaVersion"
            val shortName = "jdk@ibm-semeru"
            val opt =
              for {
                (arch, rem)      <- archOpt(assetName).toSeq
                (os, rem0)       <- osOpt(rem).toSeq
                (archiveType, _) <- archiveTypeOpt(rem0).toSeq
              } yield Seq(
                Index(os, arch, name, version, archiveType + "+" + asset.downloadUrl),
                Index(os, arch, shortName, shortVersion, archiveType + "+" + asset.downloadUrl)
              )
            opt.flatten
          }
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}
