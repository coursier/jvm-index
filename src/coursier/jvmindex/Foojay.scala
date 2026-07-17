package coursier.jvmindex

import coursier.jvmindex.Index.{Arch, Os}
import sttp.client3.quick.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.concurrent.Executors

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal

object Foojay {

  private final case class Package(
    os: Os,
    arch: Arch,
    name: String,
    version: String,
    archive: String,
    redirectUrl: String
  )

  // Distributions already fetched from their upstream sources are deliberately omitted here.
  // Keeping the mapping explicit also makes the public names in the coursier index stable if
  // Disco adds aliases or new distributions.
  private val distributionNames = Map(
    "bisheng"          -> "bisheng",
    "dragonwell"       -> "dragonwell",
    "jetbrains"        -> "jetbrains",
    "kona"             -> "kona",
    "mandrel"          -> "mandrel",
    "openlogic"        -> "openlogic",
    "oracle_open_jdk"  -> "oracle-openjdk",
    "redhat"           -> "redhat",
    "sap_machine"      -> "sapmachine",
    "semeru_certified" -> "ibm-semeru-certified"
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

  private def isExpiring(uri: URI): Boolean =
    Option(uri.getRawQuery).exists { query =>
      val keys = query.split('&').iterator.map(_.takeWhile(_ != '=')).toSet
      keys.contains("sig") && keys.contains("jwt") ||
      keys.exists(_.startsWith("X-Amz-"))
    }

  private def resolveRedirect(client: HttpClient, url: String): Option[String] =
    try {
      def follow(current: URI, previous: Option[URI], redirectsLeft: Int): Option[String] = {
        val request = HttpRequest.newBuilder(current).method(
          "HEAD",
          HttpRequest.BodyPublishers.noBody()
        ).build()
        val response = client.send(request, HttpResponse.BodyHandlers.discarding())
        if (response.statusCode() / 100 == 2)
          Some(
            if (isExpiring(current)) previous.getOrElse(current).toString
            else current.toString
          )
        else if (response.statusCode() / 100 == 3 && redirectsLeft > 0) {
          val location = response.headers().firstValue("location")
          if (location.isPresent)
            follow(current.resolve(location.get()), Some(current), redirectsLeft - 1)
          else {
            System.err.println(s"Ignoring $url (redirect without a Location header)")
            None
          }
        }
        else {
          System.err.println(s"Ignoring $url (status code ${response.statusCode()})")
          None
        }
      }
      follow(URI.create(url), None, redirectsLeft = 10)
    }
    catch {
      case NonFatal(exception) =>
        System.err.println(s"Ignoring $url (${exception.getMessage})")
        None
    }

  def index(): Index = {
    val distributions = distributionNames.keys.toVector.sorted.mkString(",")
    val url           =
      uri"https://api.foojay.io/disco/v3.0/packages/jdks?distribution=$distributions&archive_type=tar.gz&archive_type=zip&directly_downloadable=true&javafx_bundled=false&release_status=ga"
    System.err.println(s"Getting $url")
    val response = quickRequest.get(url).send(backend)
    val packages = ujson.read(response.body)("result").arr
    System.err.println(s"Found ${packages.length} Foojay packages")

    val candidates = packages.iterator.flatMap { value =>
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
        Package(os, arch, name, version, archive, redirectUrl)
      }
    }.toVector

    // Multiple Disco packages can describe the same index coordinate. Resolve only the one that
    // will be retained, preferring the archive native to the target operating system.
    val selected = candidates
      .groupBy(pkg => (pkg.os, pkg.arch, pkg.name, pkg.version))
      .valuesIterator
      .map { alternatives =>
        alternatives.minBy { pkg =>
          val preferredArchive =
            if (pkg.os == Os("windows")) "zip"
            else "tgz"
          (pkg.archive != preferredArchive, pkg.redirectUrl)
        }
      }
      .toVector

    System.err.println(s"Resolving ${selected.length} Foojay download redirects")
    val client   = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).build()
    val pool     = Executors.newFixedThreadPool(16)
    val resolved =
      try {
        implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
        Await.result(
          Future.traverse(selected) { pkg =>
            Future(resolveRedirect(client, pkg.redirectUrl).map(pkg -> _))
          },
          Duration.Inf
        )
      }
      finally pool.shutdown()

    resolved.iterator.flatten
      .map {
        case (pkg, url) =>
          Index(pkg.os, pkg.arch, s"jdk@${pkg.name}", pkg.version, s"${pkg.archive}+$url")
      }
      .foldLeft(Index.empty)(_ + _)
  }
}
