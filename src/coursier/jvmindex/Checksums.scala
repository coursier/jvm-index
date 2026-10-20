package coursier.jvmindex

import sttp.client3.quick._

import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

object Checksums {

  /** Checksum type of a hex-encoded checksum, inferred from its length */
  def checksumTypeOpt(hex: String): Option[String] =
    if (hex.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')))
      hex.length match {
        case 32  => Some("md5")
        case 40  => Some("sha1")
        case 64  => Some("sha256")
        case 128 => Some("sha512")
        case _   => None
      }
    else
      None

  /** Parses a GitHub digest, like `sha256:…` */
  def fromGitHubDigest(digest: String): Map[String, String] =
    digest.split(":", 2) match {
      case Array(checksumType, value) => Map(checksumType -> value)
      case _                          =>
        System.err.println(s"Warning: ignoring malformed GitHub digest '$digest'")
        Map.empty
    }

  /** Parses the content of a checksum file
    *
    * Checksum files contain either just a checksum, or a checksum followed by a file name, like
    * `sha256sum` outputs.
    */
  def parseChecksumFile(content: String): Option[(String, String)] = {
    val hex = content.trim.takeWhile(!_.isWhitespace).toLowerCase
    checksumTypeOpt(hex).map(_ -> hex)
  }

  /** Downloads a checksum file, and parses its content */
  def fetch(checksumFileUrl: String): Map[String, String] = {
    val res =
      try {
        val resp = quickRequest.get(uri"$checksumFileUrl").send(backend)
        if (resp.code.isSuccess)
          parseChecksumFile(resp.body).toMap
        else {
          System.err.println(s"Warning: got status code ${resp.code} for $checksumFileUrl")
          Map.empty
        }
      }
      catch {
        case NonFatal(e) =>
          System.err.println(s"Warning: error getting $checksumFileUrl: $e")
          Map.empty
      }
    if (res.isEmpty)
      System.err.println(s"Warning: no checksum found in $checksumFileUrl")
    res
  }

  /** Reads the checksums from v2 indices, keyed by archive URL */
  def fromV2Indices(dir: os.Path): Map[String, Map[String, String]] =
    if (os.isDir(dir))
      os.list(dir)
        .filter(_.last.endsWith(".json"))
        .iterator
        .flatMap { file =>
          for {
            versions <- ujson.read(os.read(file)).obj.valuesIterator
            entry    <- versions.obj.valuesIterator
            checksums = entry("checksums").obj.view.mapValues(_.str).toMap
            if checksums.nonEmpty
          } yield entry("url").str -> checksums
        }
        .toMap
    else
      Map.empty

  /** Fills the checksums of entries that only have a checksum file URL
    *
    * Checksums are taken from `known` (keyed by archive URL) if they're there, and are downloaded
    * otherwise.
    */
  def resolve(index: Index, known: Map[String, Map[String, String]]): Index = {
    val toResolve = index.entries
      .filter(_.checksums.isEmpty)
      .flatMap(e => e.checksumFileUrl.map(e.url -> _))
      .toVector
      .distinct
    val (alreadyKnown, toFetch) = toResolve.partition((url, _) => known.contains(url))
    System.err.println(
      s"Found ${alreadyKnown.length} already known checksums, fetching ${toFetch.length} checksum files"
    )

    val pool    = Executors.newFixedThreadPool(16)
    val fetched =
      try {
        implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
        val futures                       = toFetch.map { (url, checksumFileUrl) =>
          Future(url -> fetch(checksumFileUrl))
        }
        futures.map(Await.result(_, Duration.Inf)).toMap
      }
      finally
        pool.shutdown()

    val checksums = alreadyKnown.map((url, _) => url -> known(url)).toMap ++ fetched
    index.mapEntries { entry =>
      if (entry.checksums.isEmpty)
        entry.copy(checksums = checksums.getOrElse(entry.url, Map.empty))
      else
        entry
    }
  }
}
