package coursier.jvmindex

import coursier.jvmindex.Index.{Arch, Os}
import org.jsoup.Jsoup

import scala.jdk.CollectionConverters.*

object Microsoft {

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
        case Array(version, "macos", "x64") =>
          Some((Os("darwin"), Arch("amd64"), version))
        case Array(version, "windows", "x64") =>
          Some((Os("windows"), Arch("amd64"), version))
        case Array(version, "linux", "aarch64") =>
          Some((Os("linux"), Arch("arm64"), version))
        case Array(version, "alpine", "aarch64") =>
          Some((Os("linux-musl"), Arch("arm64"), version))
        case Array(version, "macos", "aarch64") =>
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
        .filter(!_.endsWith(".sha256sum.txt"))
        .filter(!_.endsWith(".sig"))
        .filter(!_.endsWith(".msi"))
        .filter(!_.endsWith(".exe"))
        .filter(!_.endsWith(".pkg"))
    }
    val latest = list("https://learn.microsoft.com/en-us/java/openjdk/download")
    val older  = list("https://learn.microsoft.com/en-us/java/openjdk/older-releases")
    val indices = (latest ++ older).flatMap(url => detailsOpt(url).iterator.map((_, url))).map {
      case ((os, arch, ver), url) =>
        val url0 =
          if (url.endsWith(".zip")) s"zip+$url"
          else if (url.endsWith(".tar.gz")) s"tgz+$url"
          else sys.error(s"Unexpected extension in $url")
        Index(os, arch, "microsoft-openjdk", ver, url0)
    }
    indices.foldLeft(Index.empty)(_ + _)
  }

}
