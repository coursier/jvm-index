package coursier.jvmindex

import Index.{Arch, Os}
import sttp.client3.quick.*

import scala.math.Ordering.Implicits.seqOrdering

object Zulu {
  final case class ZuluParams(
    indexOs: Os,
    indexArch: Arch,
    indexArchiveType: String,
    bundleType: String = "jdk",
    releaseStatus: String = "ga"
  ) {
    lazy val url: sttp.model.Uri =
      uri"https://api.azul.com/zulu/download/community/v1.0/bundles/?os=$os&arch=$arch&hw_bitness=$bitness&bundle_type=$bundleType&ext=$ext&release_status=$releaseStatus&javafx=false"

    lazy val os = indexOs match {
      case Os("linux-musl") => "linux_musl"
      case Os("darwin")     => "macos"
      case x                => x
    }

    lazy val (arch, bitness) = indexArch match {
      case Arch("arm")   => (Arch("arm"), "32")
      case Arch("arm64") => (Arch("arm"), "64")
      case Arch("x86")   => (Arch("x86"), "32")
      case Arch("amd64") => (Arch("x86"), "64")
      case Arch("ppc64") => (Arch("ppc"), "64")
      case _             => ???
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

    val oses =
      Seq(Os("darwin"), Os("linux"), Os("windows"), Os("linux-musl")) // Add "solaris", "qnx"?
    val cpus        = Seq(Arch("x86"), Arch("amd64"), Arch("arm"), Arch("arm64"), Arch("ppc64"))
    val bundleTypes = Seq("jdk", "jre")
    val allParams   = for {
      os  <- oses
      cpu <- cpus
      ext = if (os == Os("windows")) "zip" else "tgz"
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
