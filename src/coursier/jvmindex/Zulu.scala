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

    // Packages of the metadata API that may correspond to these params.
    // Unlike the v1.0 API, the metadata API gives the checksums of the packages.
    def metadataKey: (String, String, String) =
      (os.toString, ext, bundleType)

    def index(jdkVersion: Seq[Int], url: String, checksums: Map[String, String]): Index = {
      val entry = IndexEntry(indexArchiveType, url, checksums = checksums)
      Index(indexOs, indexArch, indexJdkName, jdkVersion.take(3).mkString("."), entry)
    }
  }

  /** SHA-256 checksums of Zulu packages, by download URL, from the metadata API */
  private def metadataChecksums(
    os: String,
    ext: String,
    bundleType: String
  ): Map[String, String] = {
    val pageSize                       = 1000
    def page(n: Int): Seq[ujson.Value] = {
      val url =
        uri"https://api.azul.com/metadata/v1/zulu/packages/?os=$os&archive_type=$ext&java_package_type=$bundleType&page=$n&page_size=$pageSize&include_fields=sha256_hash"
      System.err.println(s"Getting $url")
      val resp = quickRequest.get(url).send(backend)
      // Pages past the last one return a 404 error, as a JSON object
      ujson.read(resp.body).arrOpt.map(_.toSeq).getOrElse(Nil)
    }
    Iterator.from(1)
      .map(page)
      .takeWhile(_.nonEmpty)
      .flatMap(_.iterator)
      .flatMap { pkg =>
        pkg("sha256_hash").strOpt.filter(_.nonEmpty).map(pkg("download_url").str -> _)
      }
      .toMap
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

    val checksums = allParams
      .map(_.metadataKey)
      .distinct
      .map {
        case key @ (os, ext, bundleType) =>
          key -> metadataChecksums(os, ext, bundleType)
      }
      .toMap

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
            val checksums0 = checksums(params.metadataKey).get(url).map("sha256" -> _).toMap
            if (checksums0.isEmpty)
              System.err.println(s"Warning: no checksum found for $url")
            params.index(jdkVersion, url, checksums0)
          }
      }
      .foldLeft(Index.empty)(_ + _)
  }

}
