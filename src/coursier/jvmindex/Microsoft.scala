package coursier.jvmindex

import coursier.jvmindex.Index.{Arch, Os}
import org.jsoup.Jsoup

import scala.jdk.CollectionConverters.*

object Microsoft {

  // The Java 17 GA archives are named like microsoft-jdk-17.35.1-…, that is
  // feature.build.respin rather than feature.interim.update, which would sort
  // above any 17.0.x version
  private def normalizeVersion(version: String): String =
    version.split('.') match {
      case Array(feature, build, _) if build != "0" => s"$feature.0.0"
      case _                                        => version
    }

  private def detailsOpt(url: String): Option[(Os, Arch, String)] =
    if (
      url.startsWith("https://aka.ms/download-jdk/microsoft-jdk-") &&
      (url.endsWith(".tar.gz") || url.endsWith(".zip"))
    ) {
      // just in case
      assert(!url.endsWith(".tar.gz.zip"))
      assert(!url.endsWith(".zip.tar.gz"))

      val elems = url
        .stripPrefix("https://aka.ms/download-jdk/microsoft-jdk-")
        .stripSuffix(".tar.gz")
        .stripSuffix(".zip")
        .split('-')
      elems match {
        case Array(version, "linux", "x64") =>
          Some((Os("linux"), Arch("amd64"), version))
        case Array(version, "alpine", "x64") =>
          Some((Os("linux-musl"), Arch("amd64"), version))
        case Array(version, "macos" | "macOS", "x64") =>
          Some((Os("darwin"), Arch("amd64"), version))
        case Array(version, "windows", "x64") =>
          Some((Os("windows"), Arch("amd64"), version))
        case Array(version, "linux", "aarch64") =>
          Some((Os("linux"), Arch("arm64"), version))
        case Array(version, "alpine", "aarch64") =>
          Some((Os("linux-musl"), Arch("arm64"), version))
        case Array(version, "macos" | "macOS", "aarch64") =>
          Some((Os("darwin"), Arch("arm64"), version))
        case Array(version, "windows", "aarch64") =>
          Some((Os("windows"), Arch("arm64"), version))
        case Array("debugsymbols", _, _, _) =>
          // ignored
          None
        case unrecognizedMicrosoftJdkUrlElems =>
          pprint.err.log(url)
          pprint.err.log(unrecognizedMicrosoftJdkUrlElems)
          None
      }
    }
    else
      None

  def fullIndex(): Index = {
    def list(url: String): Iterator[String] = {
      val file = coursierapi.Cache.create()
        .get(coursierapi.Artifact.of(url, true, false))
      val content = os.read(os.Path(file))
      Jsoup.parse(content)
        .select("a")
        .asScala
        .iterator
        .map(_.attr("href"))
        .filter(_.startsWith("https://aka.ms/download-jdk/"))
        .filter(!_.endsWith(".sig"))
        .filter(!_.endsWith(".msi"))
        .filter(!_.endsWith(".exe"))
        .filter(!_.endsWith(".pkg"))
    }
    val latest = list("https://learn.microsoft.com/en-us/java/openjdk/download")
    val older  = list("https://learn.microsoft.com/en-us/java/openjdk/older-releases")
    val urls   = (latest ++ older).toVector
    // like https://aka.ms/download-jdk/microsoft-jdk-21.0.5-linux-x64.tar.gz.sha256sum.txt
    val checksumFileUrls = urls.filter(_.endsWith(".sha256sum.txt")).toSet
    val indices = urls.iterator.flatMap(url => detailsOpt(url).iterator.map((_, url))).map {
      case ((os, arch, ver), url) =>
        val archiveType =
          if (url.endsWith(".zip")) "zip"
          else if (url.endsWith(".tar.gz")) "tgz"
          else sys.error(s"Unexpected extension in $url")
        val entry = IndexEntry(
          archiveType,
          url,
          checksumFileUrl = Some(url + ".sha256sum.txt").filter(checksumFileUrls)
        )
        Index(os, arch, "jdk@microsoft-openjdk", normalizeVersion(ver), entry)
    }
    indices.foldLeft(Index.empty)(_ + _)
  }

}
