package coursier.jvmindex

import sttp.client3.quick._
import scala.util.control.NonFatal
import Index.{Arch, Os}

object LibericaNik {

  final case class LibericaNikEntry(
    version: String,
    jdkVersion: String,
    bitness: Int,
    os: String,
    url: String,
    bundleType: String,
    packageType: String,
    architecture: String
  ) {
    lazy val sortKey =
      (coursier.version.Version(version), coursier.version.Version(jdkVersion), packageType)

    def indexOs: Os = os match {
      case "macos" => Os("darwin")
      case x       => Os(x)
    }
    def indexArchOpt = (architecture, bitness) match {
      case ("arm", 32) => Some(Arch("arm"))
      case ("arm", 64) => Some(Arch("arm64"))
      case ("x86", 32) => Some(Arch("x86"))
      case ("x86", 64) => Some(Arch("amd64"))
      case ("ppc", 64) => Some(Arch("ppc64"))
      case _           => None
    }

    def javaVersion: Int =
      val semver = jdkVersion.split('+').head
      semver.split('.').head.toInt

    def indexJdkName = bundleType match {
      case "core"     => "jdk@liberica-nik-core"
      case "standard" => "jdk@liberica-nik"
      case "full"     => "jdk@liberica-nik-full"
      case x          => s"jdk@liberica-nik-$x"
    }

    def indexUrl = {
      val packageTypePrefix = packageType match {
        case "tar.gz" => "tgz"
        case x        => x
      }
      s"$packageTypePrefix+$url"
    }

    def indexOpt: Option[Index] =
      if packageType == "zip" || packageType == "tar.gz" then
        indexArchOpt.map { indexArch =>
          Index(indexOs, indexArch, indexJdkName, jdkVersion.takeWhile(_ != '+'), indexUrl)
        }
      else
        None
  }

  object LibericaNikEntry {
    def apply(obj: ujson.Obj): Option[LibericaNikEntry] =
      val jdkVersionOpt = obj("components")
        .arr
        .filter(_.obj("component").str == "liberica")
        .headOption
        .map(_.obj("version").str)

      jdkVersionOpt.map { jdkVersion =>
        LibericaNikEntry(
          version = obj("version").str.split('+').head,
          jdkVersion = jdkVersion,
          bitness = obj("bitness").num.toInt,
          os = obj("os").str,
          url = obj("downloadUrl").str,
          bundleType = obj("bundleType").str,
          packageType = obj("packageType").str,
          architecture = obj("architecture").str
        )
      }
  }

  def index(): Index = {

    val url = uri"https://api.bell-sw.com/v1/nik/releases"
    System.err.println(s"Getting $url")
    val ua =
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_5) AppleWebKit/601.7.8 (KHTML, like Gecko) Version/9.1.3 Safari/537.86.7"
    val resp = quickRequest
      .header("User-Agent", ua)
      .get(url)
      .send(backend)
    val json =
      try ujson.read(resp.body)
      catch {
        case NonFatal(e) =>
          System.err.println(s"Error parsing '${resp.body}'")
          throw e
      }

    val count = json.arr.length
    System.err.println(s"Found $count elements")

    json
      .arr
      .toArray
      .flatMap(elem => LibericaNikEntry(elem.obj).toSeq)
      .sortBy(_.sortKey)
      .iterator
      .flatMap(_.indexOpt.iterator)
      .foldLeft(Index.empty)(_ + _)
  }

}
