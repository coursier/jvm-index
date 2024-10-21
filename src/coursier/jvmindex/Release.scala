package coursier.jvmindex

import scala.util.control.NonFatal

final case class Release(
  tagName: String,
  prerelease: Boolean
)

object Release {

  def releaseIds(
    ghOrg: String,
    ghProj: String,
    ghToken: String
  ): Iterator[Release] = {
    def helper(before: Option[String]): Iterator[Release] = {
      System.err.println(s"Getting releases of $ghOrg/$ghProj${before.fold("")(" before " + _)} …")
      val resp = GitHub.queryRepo(ghOrg, ghProj, ghToken) {
        s"""|releases(
            |  ${before.fold("")(cursor => s"before: \"$cursor\"")}
            |  orderBy: {field: CREATED_AT, direction: DESC}
            |  last: 100
            |) {
            |  nodes { tagName isPrerelease }
            |  pageInfo { hasPreviousPage, startCursor }
            |}""".stripMargin
      }

      if (resp.isNull)
        Iterator.empty
      else {
        val json = resp("releases")
        val res =
          try json("nodes").arr.map { obj =>
              Release(obj("tagName").str, obj("isPrerelease").bool)
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
    }

    helper(None)
  }

}
