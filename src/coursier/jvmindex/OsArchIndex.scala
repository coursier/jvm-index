package coursier.jvmindex

final case class OsArchIndex(map: Map[String, Map[String, IndexEntry]]) {
  def json: String =
    Index.json2(map, _.v1Value).render(indent = 2)
  def jsonV2: String =
    Index.json2(map, _.v2Value).render(indent = 2)
}
