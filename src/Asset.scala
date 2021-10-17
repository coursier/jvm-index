import sttp.client.quick._

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
    releaseId: Long
  ): Iterator[Asset] = {

    def helper(page: Int): Iterator[Asset] = {
      val url =
        uri"https://api.github.com/repos/$ghOrg/$ghProj/releases/$releaseId/assets?page=$page"
      System.err.println(s"Getting $url")
      val resp = quickRequest
        .headers {
          if (ghToken.isEmpty) Map[String, String]()
          else Map("Authorization" -> s"token $ghToken")
        }
        .get(url)
        .send()
      val json = ujson.read(resp.body)

      val linkHeader = resp.header("Link")
      val hasNext = linkHeader
        .toSeq
        .flatMap(_.split(','))
        .exists(_.endsWith("; rel=\"next\""))

      val res =
        try json.arr.toVector.map { obj =>
          Asset(obj("name").str, obj("browser_download_url").str)
        }
        catch {
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
