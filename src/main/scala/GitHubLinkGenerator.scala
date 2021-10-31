import cats.effect.IO
import org.http4s._
import org.http4s.implicits._

object GitHubLinkGenerator {

  private val baseUrl: IO[Uri] = IO.pure(uri"https://api.github.com")

  def orgRepositories(orgName: String, perPage: Int, pageNumber: Int): IO[Uri] =
    baseUrl.map { base =>
      (base / "orgs" / orgName / "repos")
        .setQueryParams(
          Map(
            "per_page" -> List(perPage),
            "page"     -> List(pageNumber)
          )
        )
    }

  def repositoryContributors(orgName: String, repoName: String): IO[Uri] =
    baseUrl.map(_ / "repos" / orgName / repoName / "contributors")

}
