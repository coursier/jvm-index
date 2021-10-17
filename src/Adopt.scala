object Adopt {

  def fullIndex(ghToken: String): Index = {
    val adoptIndices = (8 to 16).map { num =>
      val versionPrefix = if (num == 8) "1." else ""
      index(ghToken, num, versionPrefix)
    }
    adoptIndices.foldLeft(Index.empty)(_ + _)
  }

  def index(
    ghToken: String,
    baseVersion: Int,
    versionPrefix: String = ""
  ): Index = {
    val ghOrg  = "AdoptOpenJDK"
    val ghProj = s"openjdk$baseVersion-binaries"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    val releaseJdkName = "jdk@adopt"
    val releaseAssetNamePrefix = {
      val jdkStr = "jdk"
      if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
      else s"OpenJDK$baseVersion-${jdkStr}_"
    }

    val debugJdkName = "jdk@adopt-debugimage"
    val debugAssetNamePrefix = {
      val jdkStr = "debugimage"
      if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
      else s"OpenJDK$baseVersion-${jdkStr}_"
    }

    val testJdkName = "jdk@adopt-testimage"
    val testAssetNamePrefix = {
      val jdkStr = "testimage"
      if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
      else s"OpenJDK$baseVersion-${jdkStr}_"
    }

    val jreName = "jdk@adopt-jre"
    val jreAssetNamePrefix = {
      val jdkStr = "jre"
      if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
      else s"OpenJDK$baseVersion-${jdkStr}_"
    }

    def archOpt(input: String): Option[(String, String)] =
      if (input.startsWith("x64_"))
        Some(("amd64", input.stripPrefix("x64_")))
      else if (input.startsWith("x86-32_"))
        Some(("x86", input.stripPrefix("x86-32_")))
      else if (input.startsWith("aarch64_"))
        Some(("arm64", input.stripPrefix("aarch64_")))
      else if (input.startsWith("arm_"))
        Some(("arm", input.stripPrefix("arm_")))
      else if (input.startsWith("s390x_"))
        Some(("s390x", input.stripPrefix("s390x_")))
      else if (input.startsWith("ppc64_"))
        Some(("ppc64", input.stripPrefix("ppc64_")))
      else if (input.startsWith("ppc64le_"))
        Some(("ppc64le", input.stripPrefix("ppc64le_")))
      else
        None

    def osOpt(input: String): Option[(String, String)] =
      if (input.startsWith("linux_"))
        Some(("linux", input.stripPrefix("linux_")))
      else if (input.startsWith("alpine-linux_"))
        Some(("alpine-linux", input.stripPrefix("alpine-linux_")))
      else if (input.startsWith("mac_"))
        Some(("darwin", input.stripPrefix("mac_")))
      else if (input.startsWith("windows_"))
        Some(("windows", input.stripPrefix("windows_")))
      else if (input.startsWith("aix_"))
        Some(("aix", input.stripPrefix("aix_")))
      else
        None

    def archiveTypeOpt(input: String): Option[String] =
      if (input == "zip") Some("zip")
      else if (input == "tar.gz") Some("tgz")
      else None

    val prefixes =
      if (baseVersion == 8) Seq("jdk8u")
      else Seq(s"jdk-$baseVersion.", s"jdk-$baseVersion+")
    val indices = releases0
      .filter { release =>
        prefixes.exists(prefix => release.tagName.startsWith(prefix))
      }
      .flatMap { release =>
        val version0 = release.tagName.stripPrefix("jdk-").stripPrefix("jdk")
        val versionInFileName = {
          if (version0.contains("+"))
            version0.split('+') match {
              case Array(before, after) => s"${before}_${after.takeWhile(_ != '.')}"
              case _                    => version0
            }
          else version0
        }
        lazy val assets = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.releaseId).toVector
        def index(jdkName: String, assetNamePrefix: String) = assets
          .iterator
          .filter(asset => asset.name.startsWith(assetNamePrefix))
          .flatMap { asset =>
            val name0 = asset.name.stripPrefix(assetNamePrefix)
            val opt = for {
              (arch, rem) <- archOpt(name0)
              (os, rem0)  <- osOpt(rem)
              ext <- {
                val prefix = "hotspot_" + versionInFileName.filter(_ != '-') + "."
                Some(rem0)
                  .filter(_.startsWith(prefix))
                  .map(_.stripPrefix(prefix))
              }
              archiveType <- archiveTypeOpt(ext)
            } yield Index(os, arch, jdkName, "1." + version0.takeWhile(c => c != '-' && c != '+' && c != '_').replace("u", ".0-"), archiveType + "+" + asset.downloadUrl)
            opt.toSeq
          }
        def releaseIndex = index(releaseJdkName, releaseAssetNamePrefix)
        def debugIndex   = index(debugJdkName, debugAssetNamePrefix)
        def testIndex    = index(testJdkName, testAssetNamePrefix)
        def jreIndex     = index(jreName, jreAssetNamePrefix)
        releaseIndex ++ debugIndex ++ testIndex ++ jreIndex
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}
