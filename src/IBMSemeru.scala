object IBMSemeru {

  def fullIndex(ghToken: String): Index = {
    val ibmsemeruJdk11Index0 = index(ghToken, "11")
    val ibmsemeruJdk17Index0 = index(ghToken, "17")
    val ibmsemeruJdk21Index0 = index(ghToken, "21")
    ibmsemeruJdk11Index0 + ibmsemeruJdk17Index0 + ibmsemeruJdk21Index0
  }

  def index(
    ghToken: String,
    javaVersion: String
  ): Index = {

    val ghOrg  = "ibmruntimes"
    val ghProj = s"semeru${javaVersion}-binaries"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    def osOpt(input: String): Option[(String, String)] =
      input match
        case input if input.startsWith("linux")   => Some(("linux", input.stripPrefix("linux_")))
        case input if input.startsWith("mac")     => Some(("darwin", input.stripPrefix("mac_")))
        case input if input.startsWith("windows") => Some(("windows", input.stripPrefix("windows_")))
        case input if input.startsWith("aix")     => Some(("aix", input.stripPrefix("aix_")))
        case _                                    => None

    def archOpt(input: String): Option[(String, String)] =
      input match
        case input if input.startsWith("x64")     => Some(("amd64", input.stripPrefix("x64_")))
        case input if input.startsWith("aarch64") => Some(("arm64", input.stripPrefix("aarch64_")))
        case input if input.startsWith("ppc64le") => Some(("ppc64le", input.stripPrefix("ppc64le_")))
        case input if input.startsWith("ppc64")   => Some(("ppc64", input.stripPrefix("ppc64_")))
        case input if input.startsWith("s390x")   => Some(("s390x", input.stripPrefix("s390x_")))
        case _                                    => None

    def archiveTypeOpt(input: String): Option[(String, String)] =
      input match
        case input if input.endsWith(".zip")    => Some(("zip", input.stripSuffix(".zip")))
        case input if input.endsWith(".tar.gz") => Some(("tgz", input.stripSuffix(".tar.gz")))
        case _                                  => None

    val indices = releases0
      .filter(release => release.tagName.startsWith(s"jdk-$javaVersion"))
      .flatMap { release =>
        val version         = release.tagName.stripPrefix("jdk-")
        val assetNamePrefix = s"ibm-semeru-open-jdk_"
        val assets          = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.tagName)
        assets
          .filter(asset => asset.name.startsWith(assetNamePrefix))
          .flatMap { asset =>
            val name0       = asset.name.stripPrefix(assetNamePrefix)
            println(s"asset.name: ${asset.name}")
            // val nameGlobal  = "jdk@ibm-semeru-openj9"
            val nameVersion = s"jdk@ibm-semeru-openj9-java$javaVersion"
            val opt =
              for {
                (arch, rem) <- archOpt(name0)
                (os, rem0)    <- osOpt(rem)
                (archiveType, ver) <- archiveTypeOpt(rem0)
              } yield Seq(
                // Index(os, arch, nameGlobal, version, archiveType + "+" + asset.downloadUrl),
                Index(os, arch, nameVersion, version, archiveType + "+" + asset.downloadUrl)
              )
            opt.toSeq.flatten
          }
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}
