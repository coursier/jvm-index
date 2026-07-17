package coursier.jvmindex

import coursier.jvmindex.Index.{Arch, Os}
import sttp.client3.quick.*

object Foojay {

  // Distributions already fetched from their upstream sources are deliberately omitted here.
  // Keeping the mapping explicit also makes the public names in the coursier index stable if
  // Disco adds aliases or new distributions.
  private val distributionNames = Map(
    "bisheng"          -> "bisheng",
    "dragonwell"       -> "dragonwell",
    "eliya"            -> "eliya",
    "gluon_graalvm"    -> "gluon-graalvm",
    "jetbrains"        -> "jetbrains",
    "kona"             -> "kona",
    "mandrel"          -> "mandrel",
    "ojdk_build"       -> "ojdk-build",
    "openlogic"        -> "openlogic",
    "oracle_open_jdk"  -> "oracle-openjdk",
    "redhat"           -> "redhat",
    "sap_machine"      -> "sapmachine",
    "semeru_certified" -> "ibm-semeru-certified",
    "trava"            -> "trava"
  )

  private def indexOs(value: String): Option[Os] = value match {
    case "aix"                         => Some(Os("aix"))
    case "alpine_linux" | "linux_musl" => Some(Os("linux-musl"))
    case "linux"                       => Some(Os("linux"))
    case "macos"                       => Some(Os("darwin"))
    case "solaris"                     => Some(Os("solaris"))
    case "windows"                     => Some(Os("windows"))
    case _                             => None
  }

  private def indexArch(value: String): Option[Arch] = value match {
    case "aarch64" | "arm64"                       => Some(Arch("arm64"))
    case "amd64" | "x64" | "x86-64"                => Some(Arch("amd64"))
    case "arm"                                     => Some(Arch("arm"))
    case "ppc64"                                   => Some(Arch("ppc64"))
    case "ppc64el" | "ppc64le"                     => Some(Arch("ppc64le"))
    case "s390x"                                   => Some(Arch("s390x"))
    case "x86" | "i386" | "i486" | "i586" | "i686" => Some(Arch("x86"))
    case _                                         => None
  }

  private def archiveType(value: String): Option[String] = value match {
    case "tar.gz" => Some("tgz")
    case "zip"    => Some("zip")
    case _        => None
  }

  def index(): Index = {
    val distributions = distributionNames.keys.toVector.sorted.mkString(",")
    val url           =
      uri"https://api.foojay.io/disco/v3.0/packages/jdks?distribution=$distributions&archive_type=tar.gz&archive_type=zip&directly_downloadable=true&javafx_bundled=false&release_status=ga"
    System.err.println(s"Getting $url")
    val response = quickRequest.get(url).send(backend)
    val packages = ujson.read(response.body)("result").arr
    System.err.println(s"Found ${packages.length} Foojay packages")

    packages.iterator.flatMap { value =>
      val obj          = value.obj
      val distribution = obj("distribution").str
      for {
        name        <- distributionNames.get(distribution).iterator
        os          <- indexOs(obj("operating_system").str).iterator
        arch        <- indexArch(obj("architecture").str).iterator
        archive     <- archiveType(obj("archive_type").str).iterator
        redirectUrl <- obj("links").obj.get("pkg_download_redirect").iterator.map(_.str)
      } yield {
        val version = obj("java_version").str.takeWhile(_ != '+')
        Index(os, arch, s"jdk@$name", s"1.$version", s"$archive+$redirectUrl")
      }
    }.foldLeft(Index.empty)(_ + _)
  }
}
