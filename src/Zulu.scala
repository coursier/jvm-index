import sttp.client3.quick._

import scala.math.Ordering.Implicits.seqOrdering

object Zulu {
  final case class ZuluParams(
    indexOs: String,
    indexArch: String,
    indexArchiveType: String,
    bundleType: String = "jdk",
    releaseStatus: String = "ga"
  ) {
    lazy val url: sttp.model.Uri =
      uri"https://api.azul.com/zulu/download/community/v1.0/bundles/?os=$os&arch=$arch&hw_bitness=$bitness&bundle_type=$bundleType&ext=$ext&release_status=$releaseStatus&javafx=false"

    lazy val os = indexOs match {
      case "linux-musl" => "linux_musl"
      case "darwin"     => "macos"
      case x            => x
    }

    lazy val (arch, bitness) = indexArch match {
      case "arm"   => ("arm", "32")
      case "arm64" => ("arm", "64")
      case "x86"   => ("x86", "32")
      case "amd64" => ("x86", "64")
      case "ppc64" => ("ppc", "64")
      case _       => ???
    }

    lazy val ext = indexArchiveType match {
      case "tgz" => "tar.gz"
      case x     => x
    }

    lazy val indexJdkName = bundleType match {
      case "jdk" => "jdk@zulu"
      case x     => s"jdk@zulu-$x"
    }

    def index(jdkVersion: Seq[Int], url: String): Index = {
      val indexUrl = s"$indexArchiveType+$url"
      Index(indexOs, indexArch, indexJdkName, jdkVersion.take(3).mkString("."), indexUrl)
    }
  }

  def index(): Index = {

    val oses        = Seq("darwin", "linux", "windows", "linux-musl") // Add "solaris", "qnx"?
    val cpus        = Seq("x86", "amd64", "arm", "arm64", "ppc64")
    val bundleTypes = Seq("jdk", "jre")
    val allParams = for {
      os  <- oses
      cpu <- cpus
      ext = if (os == "windows") "zip" else "tgz"
      bundleType <- bundleTypes
    } yield ZuluParams(os, cpu, ext, bundleType = bundleType)

    allParams
      .flatMap { params =>
        System.err.println(s"Getting ${params.url}")
        val resp = quickRequest.get(params.url).send(backend)
        val json = ujson.read(resp.body)

        val count = json.arr.length
        System.err.println(s"Found $count elements")

        json.arr
          .toArray
          .map(_.obj)
          .map(a => a("jdk_version").arr.toList.map(_.num.toInt) -> a)
          .sortBy(_._1)
          .iterator
          .map(_._2)
          .map { obj =>
            val url        = obj("url").str
            val jdkVersion = obj("jdk_version").arr.toList.map(_.num.toInt)
            params.index(jdkVersion, url)
          }
      }
      .foldLeft(Index.empty)(_ + _)
  }

}
