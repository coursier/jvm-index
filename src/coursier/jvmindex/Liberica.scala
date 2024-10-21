package coursier.jvmindex

import sttp.client3.quick._
import scala.util.control.NonFatal
import Index.Os

object Liberica {

  final case class LibericaEntry(
    featureVersion: Int,
    patchVersion: Int,
    updateVersion: Int,
    buildVersion: Int,
    bitness: Int,
    os: String,
    url: String,
    bundleType: String,
    packageType: String,
    architecture: String
  ) {
    lazy val sortKey = (featureVersion, patchVersion, updateVersion, buildVersion, packageType)

    def jdkVersion: String = s"$featureVersion.$patchVersion.$updateVersion"

    def indexOs: Os = os match {
      case "macos" => Os("darwin")
      case x       => Os(x)
    }
    def indexArchOpt = (architecture, bitness) match {
      case ("arm", 32) => Some("arm")
      case ("arm", 64) => Some("arm64")
      case ("x86", 32) => Some("x86")
      case ("x86", 64) => Some("amd64")
      case ("ppc", 64) => Some("ppc64")
      case _           => None
    }

    def indexJdkName = bundleType match {
      case "jdk"                         => "jdk@liberica"
      case jdk if jdk.startsWith("jdk-") => "jdk@liberica" + jdk.stripPrefix("jdk")
      case x                             => s"jdk@liberica-$x"
    }

    def indexUrl = {
      val packageTypePrefix = packageType match {
        case "tar.gz" => "tgz"
        case x        => x
      }
      s"$packageTypePrefix+$url"
    }

    def indexOpt: Option[Index] =
      if (packageType == "zip" || packageType == "tar.gz")
        indexArchOpt.map { indexArch =>
          Index(indexOs, indexArch, indexJdkName, jdkVersion, indexUrl)
        }
      else
        None
  }

  object LibericaEntry {
    def apply(obj: ujson.Obj): LibericaEntry =
      LibericaEntry(
        featureVersion = obj("featureVersion").num.toInt,
        patchVersion = obj("patchVersion").num.toInt,
        updateVersion = obj("updateVersion").num.toInt,
        buildVersion = obj("buildVersion").num.toInt,
        bitness = obj("bitness").num.toInt,
        os = obj("os").str,
        url = obj("downloadUrl").str,
        bundleType = obj("bundleType").str,
        packageType = obj("packageType").str,
        architecture = obj("architecture").str
      )
  }

  def index(): Index = {

    val url = uri"https://api.bell-sw.com/v1/liberica/releases"
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
      .map(elem => LibericaEntry(elem.obj))
      .sortBy(_.sortKey)
      .iterator
      .flatMap(_.indexOpt.iterator)
      .foldLeft(Index.empty)(_ + _)
  }

}
