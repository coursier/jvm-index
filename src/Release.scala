
import sttp.client.quick._
import scala.util.control.NonFatal

final case class Release(
  releaseId: Long,
  tagName: String,
  prerelease: Boolean
)

object Release {

  def releaseIds(
    ghOrg: String,
    ghProj: String,
    ghToken: String
  ): Iterator[Release] = {

    def helper(page: Int): Iterator[Release] = {
      val url = uri"https://api.github.com/repos/$ghOrg/$ghProj/releases?page=$page"
      System.err.println(s"Getting $url")
      val resp = quickRequest
        .headers(if (ghToken.isEmpty) Map[String, String]() else Map("Authorization" -> s"token $ghToken"))
        .get(url)
        .send()
      val linkHeader = resp.header("Link")
      val hasNext = linkHeader
        .toSeq
        .flatMap(_.split(','))
        .exists(_.endsWith("; rel=\"next\""))
      val json = ujson.read(resp.body)

      val res = try {
        json.arr.toVector.map { obj =>
          Release(obj("id").num.toLong, obj("tag_name").str, obj("prerelease").bool)
        }
      } catch {
        case NonFatal(e) =>
          System.err.println(resp.body)
          throw e
      }

      if (hasNext)
        res.iterator ++ helper(page + 1)
      else
        res.iterator
    }

    helper(1)
  }

}
