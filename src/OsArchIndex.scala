final case class OsArchIndex(map: Map[String, Map[String, String]]) {
  def json: String =
    Index.json2(map).render(indent = 2)
}
