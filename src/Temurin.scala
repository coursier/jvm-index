import Index.Os

object Temurin {

  def fullIndex(ghToken: String): Index = {
    val adoptIndices = (8 to 16).map(ver => ver -> index(ghToken, ver, adopt = true))
    val temurinIndices = Seq(8, 11, 16, 17, 18, 19, 20, 21)
      .map(ver => ver -> index(ghToken, ver, adopt = false))

    val adoptiumIndices = (adoptIndices.toMap ++ temurinIndices)
      .toVector
      .sortBy(_._1)
      .map(_._2)
      .map { index0 =>
        index0.mapJdkName { name =>
          val suffix = name
            .stripPrefix("jdk@adopt")
            .stripPrefix("jdk@temurin")
          "jdk@adoptium" + suffix
        }
      }

    val allIndices = adoptIndices.iterator.map(_._2) ++
      temurinIndices.iterator.map(_._2) ++
      adoptiumIndices.iterator
    allIndices.foldLeft(Index.empty)(_ + _)
  }

  def index(
    ghToken: String,
    baseVersion: Int,
    adopt: Boolean
  ): Index = {
    val ghOrg         = if (adopt) "AdoptOpenJDK" else "adoptium"
    val projectPrefix = if (adopt) "openjdk" else "temurin"
    val ghProj        = s"$projectPrefix$baseVersion-binaries"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    def jdkName(suffix: String = ""): String =
      "jdk@" + (if (adopt) "adopt" else "temurin") + suffix

    def assetNamePrefix(jdkStr: String) = Seq(
      s"OpenJDK${baseVersion}U-${jdkStr}_",
      s"OpenJDK$baseVersion-${jdkStr}_"
    )

    def archOpt(input: String): Option[(String, String)] =
      Map(
        "amd64"   -> "x64_",
        "x86"     -> "x86-32_",
        "arm64"   -> "aarch64_",
        "arm"     -> "arm_",
        "s390x"   -> "s390x_",
        "ppc64"   -> "ppc64_",
        "ppc64le" -> "ppc64le_"
      ).collectFirst {
        case (k, v) if input.startsWith(v) =>
          k -> input.stripPrefix(v)
      }

    def osOpt(input: String): Option[(Os, String)] =
      if (input.startsWith("linux_"))
        Some((Os("linux"), input.stripPrefix("linux_")))
      else if (input.startsWith("alpine-linux_"))
        Some((Os("alpine-linux"), input.stripPrefix("alpine-linux_")))
      else if (input.startsWith("mac_"))
        Some((Os("darwin"), input.stripPrefix("mac_")))
      else if (input.startsWith("windows_"))
        Some((Os("windows"), input.stripPrefix("windows_")))
      else if (input.startsWith("aix_"))
        Some((Os("aix"), input.stripPrefix("aix_")))
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
        val versionInFileName =
          if (version0.contains("+"))
            version0.split('+') match {
              case Array(before, after) => s"${before}_${after.takeWhile(_ != '.')}"
              case _                    => version0
            }
          else version0
        val assets = Asset.releaseAssets(ghOrg, ghProj, ghToken, release.tagName).to(LazyList)
        def index(jdkName: String, assetNamePrefix: Seq[String]) = assets
          .iterator
          .filter(asset => assetNamePrefix.exists(asset.name.startsWith))
          .flatMap { asset =>
            val name0 = assetNamePrefix.foldLeft(asset.name)(_ stripPrefix _)
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
        def releaseIndex = index(jdkName(), assetNamePrefix("jdk"))
        def debugIndex   = index(jdkName("-debugimage"), assetNamePrefix("debugimage"))
        def testIndex    = index(jdkName("-testimage"), assetNamePrefix("testimage"))
        def jreIndex     = index(jdkName("-jre"), assetNamePrefix("jre"))
        releaseIndex ++ debugIndex ++ testIndex ++ jreIndex
      }

    indices.foldLeft(Index.empty)(_ + _)
  }

}
