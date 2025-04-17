package coursier.jvmindex

import Index.{Arch, Os}

object Temurin {

  def fullIndex(ghToken: String): Index = {
    val adoptIndices = (8 to 16).map(ver => ver -> index(ghToken, ver, adopt = true))
    val temurinIndices = (Iterator(8, 11) ++ Iterator.from(16))
      .map(ver => ver -> index(ghToken, ver, adopt = false))
      .takeWhile(!_._2.isEmpty)
      .toVector

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

    def archOpt(input: String): Option[(Arch, String)] =
      Map(
        Arch("amd64")   -> "x64_",
        Arch("x86")     -> "x86-32_",
        Arch("arm64")   -> "aarch64_",
        Arch("arm")     -> "arm_",
        Arch("s390x")   -> "s390x_",
        Arch("ppc64")   -> "ppc64_",
        Arch("ppc64le") -> "ppc64le_"
      ).collectFirst {
        case (k, v) if input.startsWith(v) =>
          k -> input.stripPrefix(v)
      }

    def oses(input: String): Seq[(Os, String)] =
      if (input.startsWith("linux_"))
        Seq((Os("linux"), input.stripPrefix("linux_")))
      else if (input.startsWith("alpine-linux_")) {
        val remaining = input.stripPrefix("alpine-linux_")
        Seq(
          (Os("alpine-linux"), remaining), // deprecated, use linux-musl instead
          (Os("linux-musl"), remaining)
        )
      }
      else if (input.startsWith("mac_"))
        Seq((Os("darwin"), input.stripPrefix("mac_")))
      else if (input.startsWith("windows_"))
        Seq((Os("windows"), input.stripPrefix("windows_")))
      else if (input.startsWith("aix_"))
        Seq((Os("aix"), input.stripPrefix("aix_")))
      else
        Nil

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
            for {
              (arch, rem) <- archOpt(name0).toSeq
              (os, rem0)  <- oses(rem)
              ext <- {
                val prefix = "hotspot_" + versionInFileName.filter(_ != '-') + "."
                Seq(rem0)
                  .filter(_.startsWith(prefix))
                  .map(_.stripPrefix(prefix))
              }
              archiveType <- archiveTypeOpt(ext).toSeq
            } yield Index(
              os,
              arch,
              jdkName,
              "1." + version0.takeWhile(c => c != '-' && c != '+' && c != '_').replace("u", ".0-"),
              archiveType + "+" + asset.downloadUrl
            )
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
