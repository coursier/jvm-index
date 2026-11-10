package coursier.jvmindex

import Index.{Arch, Os}

/** Mandrel, Red Hat's downstream distribution of GraalVM community, focused on `native-image`
  *
  * Releases are tagged like `mandrel-25.0.5.0-Final` (or `mandrel-20.3.0.0.Final` for older ones),
  * and a single release can ship JDKs for several Java versions (like `mandrel-java17-…` and
  * `mandrel-java20-…`). Entries are added as `jdk@mandrel-java$javaVersion`, versioned with the
  * Mandrel version (like `25.0.5.0`).
  *
  * Entries are also added as `jdk@mandrel`, versioned with the version of the JDK Mandrel is based
  * on (like `25.0.5`). That version is read from the release notes, that say things like "using the
  * standard OpenJDK project release of jdk 25.0.5+8-LTS". The release notes of older releases don't
  * have it ("… of jdk11u"), so these are only available as `jdk@mandrel-java$javaVersion`. For more
  * recent releases, failing to read it results in a warning, that the index update workflow posts
  * on its pull request, so that changes in the format of the release notes don't go unnoticed. If
  * several Mandrel releases are based on the same JDK version, `jdk@mandrel` points to the most
  * recent one.
  */
object Mandrel {

  private def ghOrg  = "graalvm"
  private def ghProj = "mandrel"

  private val tagRegex   = """mandrel-(\d+(?:\.\d+)*)([-.])Final""".r
  private val assetRegex =
    """mandrel-java(\d+)-([a-z]+)-([a-z0-9_]+)-(\d+(?:\.\d+)*)[-.]Final\.(tar\.gz|zip)""".r

  private def jdkVersionsMarker = "OpenJDK project release of"
  // like "21.0.13+9-LTS" or "25+36-LTS"
  private val fullJdkVersionRegex = """(\d+(?:\.\d+)*)\+\d+""".r
  // like "jdk 21." or "[JDK 24](…)", but not "jdk11u"
  private val gaJdkVersionRegex = """(?i)jdk[ -]*(\d+)(?![\du+]|\.\d)""".r
  // like "jdk11u and jdk17u", in the release notes of older releases
  private val legacyJdkVersionRegex = """(?i)\bjdk\d+u\b""".r

  /** JDK versions a Mandrel release is based on, read from its release notes, by major version
    *
    * Returns versions like `21 -> "21.0.13"`, or `24 -> "24.0.0"` for GA versions. Returns `None`
    * for older releases, whose release notes only say things like "jdk11u". Throws if the release
    * notes don't say which JDK the release is based on.
    */
  def jdkVersions(releaseNotes: String): Option[Map[String, String]] = {
    val relevant = releaseNotes
      .linesIterator
      .flatMap { line =>
        val idx = line.indexOf(jdkVersionsMarker)
        if (idx < 0) None
        else Some(line.substring(idx + jdkVersionsMarker.length))
      }
      .mkString("\n")
    if (relevant.isEmpty)
      sys.error(s"'$jdkVersionsMarker' not found in release notes")
    if (legacyJdkVersionRegex.findFirstIn(relevant).isDefined) None
    else Some(jdkVersions0(relevant))
  }

  private def jdkVersions0(relevant: String): Map[String, String] = {
    val fullVersions = fullJdkVersionRegex
      .findAllMatchIn(relevant)
      .map(_.group(1))
      .map { version =>
        val parts = version.split('.')
        if (parts.length < 3) (parts ++ Array.fill(3 - parts.length)("0")).mkString(".")
        else version
      }
      .toVector
    val versions =
      if (fullVersions.isEmpty)
        gaJdkVersionRegex
          .findAllMatchIn(relevant)
          .map(_.group(1) + ".0.0")
          .toVector
      else
        fullVersions
    versions
      .distinct
      .groupBy(_.takeWhile(_ != '.'))
      .collect {
        case (major, Seq(version)) => major -> version
      }
  }

  private def osOpt(input: String): Option[Os] =
    input match {
      case "linux"   => Some(Os("linux"))
      case "macos"   => Some(Os("darwin"))
      case "windows" => Some(Os("windows"))
      case _         => None
    }

  private def archOpt(input: String): Option[Arch] =
    input match {
      case "amd64"   => Some(Arch("amd64"))
      case "aarch64" => Some(Arch("arm64"))
      case _         => None
    }

  private def archiveTypeOpt(input: String): Option[String] =
    input match {
      case "tar.gz" => Some("tgz")
      case "zip"    => Some("zip")
      case _        => None
    }

  /** Index of all Mandrel releases, along with warnings about them
    *
    * Warnings are about releases whose release notes can't be read as expected. These are still in
    * the index, but only as `jdk@mandrel-java$javaVersion`, not as `jdk@mandrel`.
    */
  def fullIndex(ghToken: String): (Index, Seq[String]) = {

    // Some versions were released twice, as both `mandrel-X.Final` and later as
    // `mandrel-X-Final` (re-spins). We keep only the latter in that case.
    val releases = Release.releaseIds(ghOrg, ghProj, ghToken, withDescription = true)
      .filter(!_.prerelease)
      .toVector
      .collect {
        case release @ Release(tagRegex(version, sep), _, _) =>
          (version, sep == "-", release)
      }
      .groupBy(_._1)
      .toVector
      .map {
        case (version, entries) =>
          version -> entries.maxBy(_._2)._3
      }
      // sorting by Mandrel version, so that the most recent Mandrel release wins
      // when several ones are based on the same JDK version
      .sortBy((version, _) => coursier.version.Version(version))

    val results = releases.map { (version, release) =>
      index(ghToken, version, release.tagName, release.description)
    }

    val warnings = results.flatMap(_._2)
    for (warning <- warnings)
      System.err.println(s"Warning: $warning")

    (results.map(_._1).foldLeft(Index.empty)(_ + _), warnings)
  }

  /** Index of a Mandrel release, along with a warning if its release notes can't be read */
  def index(
    ghToken: String,
    version: String,
    tagName: String,
    releaseNotes: String
  ): (Index, Option[String]) = {
    val assets       = Asset.releaseAssets(ghOrg, ghProj, ghToken, tagName).toVector
    val assetsByName = assets.map(asset => asset.name -> asset).toMap

    val javaVersions = assets.map(_.name).collect {
      case assetRegex(javaVersion, _, _, assetVersion, _) if assetVersion == version =>
        javaVersion
    }.toSet

    val jdkVersionsOrError: Either[String, Map[String, String]] =
      if (javaVersions.isEmpty)
        Left(s"No Mandrel archive found in release $tagName")
      else
        try
          Mandrel.jdkVersions(releaseNotes) match {
            case None                                                    => Right(Map.empty)
            case Some(jdkVersions) if javaVersions == jdkVersions.keySet => Right(jdkVersions)
            case Some(jdkVersions)                                       =>
              val mentioned =
                if (jdkVersions.isEmpty) "(none)"
                else jdkVersions.toVector.sorted.map(_._2).mkString(", ")
              Left(
                s"Mandrel release $tagName has archives for Java " +
                  s"${javaVersions.toVector.sorted.mkString(", ")}, but its release notes " +
                  s"mention JDK versions $mentioned. Has the format of the release notes changed?"
              )
          }
        catch {
          case e: RuntimeException =>
            Left(s"Error reading release notes of Mandrel release $tagName: ${e.getMessage}")
        }
    val jdkVersions = jdkVersionsOrError.getOrElse(Map.empty)

    val index = assets
      .flatMap { asset =>
        asset.name match {
          case assetRegex(javaVersion, os0, arch0, assetVersion, ext) if assetVersion == version =>
            val opt = for {
              os          <- osOpt(os0)
              arch        <- archOpt(arch0)
              archiveType <- archiveTypeOpt(ext)
            } yield {
              val entry = Asset.indexEntry(asset, archiveType, assetsByName)
              Index(os, arch, s"jdk@mandrel-java$javaVersion", version, entry) +:
                jdkVersions.get(javaVersion).toSeq.map { jdkVersion =>
                  Index(os, arch, "jdk@mandrel", jdkVersion, entry)
                }
            }
            opt.toSeq.flatten
          case _ =>
            Nil
        }
      }
      .foldLeft(Index.empty)(_ + _)

    (index, jdkVersionsOrError.left.toOption)
  }

}
