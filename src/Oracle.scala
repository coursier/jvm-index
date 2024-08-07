import sttp.client3.quick._
import Index.Os

object Oracle {
  final case class Params(
    indexOs: Os,
    indexArch: String,
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
      case "aarch64" => "arm64"
      case "x64"     => "amd64"
      case _         => ???
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
    val jdks     = Seq("17", "21")
    val jdkNames = Seq("java", "graalvm")
    val allParams = for {
      os      <- oses
      cpu     <- if (os == Os("windows")) Seq("x64") else Seq("x64", "aarch64")
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
