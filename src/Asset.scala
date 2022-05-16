import scala.util.control.NonFatal

final case class Asset(
  name: String,
  downloadUrl: String
)

object Asset {

  def releaseAssets(
    ghOrg: String,
    ghProj: String,
    ghToken: String,
    tagName: String
  ): Iterator[Asset] = {

    def helper(before: Option[String]): Iterator[Asset] = {
      System.err.println(
        s"Getting assets of $ghOrg/$ghProj for release $tagName${before.fold("")(" before " + _)} …"
      )
      val resp = GitHub.queryRepo(ghOrg, ghProj, ghToken) {
        s"""|release(tagName: "$tagName") {
            |  releaseAssets(
            |    ${before.fold("")(cursor => s"before: \"$cursor\"")}
            |    last: 100
            |  ) {
            |    nodes { name downloadUrl }
            |    pageInfo { hasPreviousPage, startCursor }
            |  }
            |}""".stripMargin
      }
      val json = resp("release")("releaseAssets")

      val res =
        try json("nodes").arr.map { obj =>
            Asset(obj("name").str, obj("downloadUrl").str)
          }
        catch {
          case NonFatal(e) =>
            System.err.println(json)
            throw e
        }

      val pageInfo = json("pageInfo")
      if (pageInfo("hasPreviousPage").bool)
        res.iterator ++ helper(Some(pageInfo("startCursor").str))
      else
        res.iterator
    }

    helper(None)
  }

}
