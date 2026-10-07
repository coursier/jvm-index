package coursier.jvmindex

import coursier.version.Version
import sttp.client3.quick._
import Index.{Arch, Os}

import scala.util.control.NonFatal

object JetBrains {

  private def ghOrg  = "JetBrains"
  private def ghProj = "JetBrainsRuntime"

  // JetBrains Runtime releases don't have GitHub assets: download links are
  // listed in tables in the release descriptions instead
  private final case class JbrRelease(
    tagName: String,
    prerelease: Boolean,
    description: String
  )

  private def releases(ghToken: String): Iterator[JbrRelease] = {
    def helper(before: Option[String]): Iterator[JbrRelease] = {
      System.err.println(s"Getting releases of $ghOrg/$ghProj${before.fold("")(" before " + _)} …")
      val resp = GitHub.queryRepo(ghOrg, ghProj, ghToken) {
        s"""|releases(
            |  ${before.fold("")(cursor => s"before: \"$cursor\"")}
            |  orderBy: {field: CREATED_AT, direction: DESC}
            |  last: 50
            |) {
            |  nodes { tagName isPrerelease description }
            |  pageInfo { hasPreviousPage, startCursor }
            |}""".stripMargin
      }

      val json = resp("releases")
      val res  =
        try json("nodes").arr.map { obj =>
            JbrRelease(
              obj("tagName").str,
              obj("isPrerelease").bool,
              obj("description").strOpt.getOrElse("")
            )
          }
        catch {
          case NonFatal(e) =>
            System.err.println(json)
            throw e
        }

      val pageInfo = json("pageInfo")
      if (pageInfo("hasPreviousPage").bool)
        res.iterator ++ helper(Some(pageInfo("startCursor").str))
      else
        res.iterator
    }

    helper(None)
  }

  private val urlRegex = """https://cache-redirector\.jetbrains\.com/intellij-jbr/[^)\s]+""".r

  // Like jbrsdk_jcef-21.0.11-linux-x64-b1163.116.tar.gz or jbr-11_0_16-osx-aarch64-b2043.64.tar.gz.
  // fastdebug builds, debug symbols (_diz / _pdb), and macOS .pkg installers don't match this.
  private val fileNameRegex =
    """(jbr|jbrsdk)(_jcef)?-([0-9][0-9._]*)-(linux-musl|linux|osx|windows)-(x64|x86|aarch64)-b([0-9.]+)\.(tar\.gz|zip)""".r

  private final case class Entry(
    os: Os,
    arch: Arch,
    jdkName: String,
    version: String,
    build: String,
    archiveType: String,
    url: String
  )

  private def entryOpt(url: String): Option[Entry] = {
    val fileName = url.substring(url.lastIndexOf('/') + 1)
    fileName match {
      case fileNameRegex(kind, jcef, version, os, arch, build, ext) =>
        val os0 = os match {
          case "osx" => Os("darwin")
          case other => Os(other)
        }
        val arch0 = arch match {
          case "x64"     => Arch("amd64")
          case "aarch64" => Arch("arm64")
          case other     => Arch(other)
        }
        val jdkName = "jdk@jetbrains" +
          (if (jcef == null) "" else "-jcef") +
          (if (kind == "jbr") "-jre" else "")
        val archiveType = if (ext == "zip") "zip" else "tgz"
        Some(Entry(os0, arch0, jdkName, version.replace('_', '.'), build, archiveType, url))
      case _ =>
        None
    }
  }

  private def isAvailable(url: String): Boolean = {
    val resp = quickRequest.head(uri"$url").send(backend)
    resp.code.code match {
      case 200       => true
      case 403 | 404 =>
        System.err.println(s"Warning: $url not found (${resp.code.code}), ignoring it")
        false
      case other =>
        sys.error(s"Unexpected status code $other when checking $url")
    }
  }

  def fullIndex(ghToken: String): Index = {

    val entries = releases(ghToken)
      .filter(!_.prerelease)
      .flatMap(release => urlRegex.findAllIn(release.description))
      .flatMap(entryOpt(_).iterator)
      .toVector

    // The same JDK version can be published in several releases, with different build numbers.
    // We only keep the most recent build that can actually be downloaded (some archives,
    // like a few x86 ones, are listed in release descriptions but were never uploaded).
    // On Windows, both zip and tar.gz archives are sometimes available, and we prefer
    // zip archives, like for other JDKs.
    val retained = entries
      .groupBy(e => (e.os, e.arch, e.jdkName, e.version))
      .valuesIterator
      .flatMap { candidates =>
        candidates
          .sortBy(e => (Version(e.build), e.archiveType == "zip"))
          .reverseIterator
          .find(e => isAvailable(e.url))
      }

    retained
      .map(e => Index(e.os, e.arch, e.jdkName, e.version, s"${e.archiveType}+${e.url}"))
      .foldLeft(Index.empty)(_ + _)
  }

}
