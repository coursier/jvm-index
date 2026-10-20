package coursier.jvmindex

/** A JVM archive in the index
  *
  * @param archiveType
  *   `tgz` or `zip`
  * @param url
  *   URL of the archive
  * @param checksums
  *   checksums of the archive, like `"sha256" -> "…"`
  * @param checksumFileUrl
  *   URL of a file containing the checksum of the archive, downloaded by [[Checksums.resolve]] if
  *   `checksums` is empty
  */
final case class IndexEntry(
  archiveType: String,
  url: String,
  checksums: Map[String, String] = Map.empty,
  checksumFileUrl: Option[String] = None
) {

  /** Value of this entry in the v1 index, like `tgz+https://…` */
  def v1Value: ujson.Value =
    ujson.Str(s"$archiveType+$url")

  /** Value of this entry in the v2 index, like `{"url": "https://…", "checksums": {…}}` */
  def v2Value: ujson.Value =
    ujson.Obj(
      "url"       -> ujson.Str(url),
      "checksums" -> ujson.Obj.from(checksums.toVector.sorted.map((k, v) => k -> ujson.Str(v)))
    )
}
