package coursier.jvmindex

import sttp.client3.quick._
import Index.{Arch, Os}

/*
- Latest Corretto binaries are listed at https://docs.aws.amazon.com/corretto/
  - Previous tags for various JDK versions are in GitHub
    - See all Corretto JDK versions: https://github.com/orgs/corretto/repositories?q=corretto,
    - See all tag names for a given JDK version: e.g. https://github.com/corretto/corretto-17/releases
  - In GitHub releases, artefacts are stored as links to downloads on the Corretto website:
    - e.g. https://corretto.aws/downloads/resources/17.0.6.10.1/amazon-corretto-17.0.6.10.1-linux-x64.tar.gz
- Based on this, we used a mix of approaches based on e.g. Graalvm (GitHub path) and Zulu (try combinations of downloads)
  - 1. use GitHub to get the tag names for a given JDK version
  - 2. Make requests to the Corretto site, to check for valid combinations
  - This lets us verify downloads are valid
    - Invalid combinations give HTTP 301, and a redirect (if enabled)
    - Valid combinations give HTTP 200, and we can ignore the download to move on quickly
 */
object Corretto {

  final case class CorrettoParams(
    indexOs: Os,
    indexArch: Arch,
    indexArchiveType: String
  ) {
    lazy val os = indexOs match {
      case Os("linux-musl") => "alpine-linux"
      case Os("darwin")     => "macosx"
      case x                => x
    }

    lazy val ext = indexArchiveType match {
      case "tgz" => "tar.gz"
      case x     => x
    }

    lazy val arch = indexArch match {
      case Arch("arm64") => "aarch64"
      case Arch("amd64") => "x64"
      case _             => ???
    }

    lazy val jdk = indexOs match {
      case Os("windows") => "-jdk"
      case _             => ""
    }

    def index(jdkTagVersion: String, url: String): Index =
      Index(indexOs, indexArch, "jdk@corretto", jdkTagVersion, url)
  }

  def fullIndex(ghToken: String): Index =
    (Iterator("8", "11") ++ Iterator.from(17).map(_.toString))
      .map(v => index(ghToken, v))
      .takeWhile(!_.isEmpty)
      .foldLeft(Index.empty)(_ + _)

  def index(
    ghToken: String,
    javaVersion: String
  ): Index = {
    val ghOrg  = "corretto"
    val ghProj = s"corretto-$javaVersion"
    val releases0 = Release.releaseIds(ghOrg, ghProj, ghToken)
      .filter(!_.prerelease)

    releases0
      .flatMap { release =>
        // See https://github.com/corretto/corretto-17/releases/tag/17.0.6.10.1 for os/cpu combinations
        val oses = Seq(
          Os("darwin"),
          Os("linux"),
          Os("windows"),
          Os("alpine-linux"), // deprecated, use linux-musl instead
          Os("linux-musl")
        )
        val cpus = Seq(Arch("amd64"), Arch("arm64"))
        val allParams = for {
          os  <- oses
          cpu <- cpus
          ext = if (os == Os("windows")) "zip" else "tgz"
        } yield CorrettoParams(os, cpu, ext)

        allParams
          .flatMap { params =>
            // url is like:
            // https://corretto.aws/downloads/resources/17.0.4.9.1/amazon-corretto-17.0.4.9.1-alpine-linux-x64.tar.gz
            val url: sttp.model.Uri =
              uri"https://corretto.aws/downloads/resources/${release.tagName}/amazon-corretto-${release.tagName}-${params.os}-${params.arch}${params.jdk}.${params.ext}"
            val resp = quickRequest.get(url)
              .followRedirects(false) // invalid URL => 301 + redirect to 200; keep the 301
              .response(ignore)       // don't download and hang on 200s
              .send(backend)
            val code = resp.code

            if (code.isSuccess) {
              System.err.println(s"Valid url (status code $code): $url")
              Some(params.index(
                jdkTagVersion = release.tagName,
                url = s"${params.indexArchiveType}+${url.toString}"
              ))
            }
            else {
              System.err.println(s"Invalid url (status code $code): $url")
              None
            }
          }                          // have list of indexes, for one jdk tag
      }.foldLeft(Index.empty)(_ + _) // combining all indexes for all jdk tags
  }                                  // all indexing done
}
