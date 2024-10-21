package coursier.jvmindex

import sttp.client3.quick._
import Index.{Arch, Os}

object Oracle {
  final case class Params(
    indexOs: Os,
    indexArch: Arch,
    indexJdkName: String,
    jdkVersion: String,
    indexArchiveType: String
  ) {
    lazy val url: sttp.model.Uri =
      uri"https://download.oracle.com/$indexJdkName/$jdkVersion/latest/$jdkName-${jdkVersion}_$os-${indexArch}_bin.$ext"

    lazy val jdkName = indexJdkName match {
      case "java"    => "jdk"
      case "graalvm" => "graalvm-jdk"
    }

    lazy val os = indexOs match {
      case Os("linux")   => "linux"
      case Os("darwin")  => "macos"
      case Os("windows") => "windows"
      case x             => x
    }

    lazy val arch = indexArch match {
      case Arch("aarch64") => Arch("arm64")
      case Arch("x64")     => Arch("amd64")
      case _               => ???
    }

    lazy val ext = indexArchiveType match {
      case "tgz" => "tar.gz"
      case x     => x
    }

    def index(url: String) =
      val indexUrl = s"$indexArchiveType+$url"
      Index(indexOs, arch, s"jdk@$indexJdkName-oracle", jdkVersion, indexUrl)
  }

  def index(): Index = {
    val oses     = Seq(Os("darwin"), Os("linux"), Os("windows"))
    val jdks     = Seq("21", "23")
    val jdkNames = Seq("java", "graalvm")
    val allParams = for {
      os      <- oses
      cpu     <- if (os == Os("windows")) Seq(Arch("x64")) else Seq(Arch("x64"), Arch("aarch64"))
      jdk     <- jdks
      jdkName <- jdkNames
      ext = if (os == Os("windows")) "zip" else "tgz"
    } yield Params(os, cpu, jdkName, jdk, ext)

    allParams
      .map { params =>
        val resp = quickRequest.get(params.url)
          .followRedirects(false) // invalid URL => 301 + redirect to 200; keep the 301
          .response(ignore)       // don't download and hang on 200s
          .send(backend)

        if (resp.code.isSuccess) {
          System.err.println(s"Valid url (status code ${resp.code}): ${params.url}")
          params.index(params.url.toString)
        }
        else {
          System.err.println(s"Invalid url (status code ${resp.code}): ${params.url}")
          Index.empty
        }
      }
      .filter(_ != Index.empty)
      .foldLeft(Index.empty)(_ + _)
  }

}
