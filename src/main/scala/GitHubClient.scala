import ClientModel.GitHubProtocol
import ServerModel.ContributorResponseDTO
import cats.effect.IO
import cats.syntax.traverse._
import fs2.Stream
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, Headers, Request, Uri}

object GitHubClient {
  private val token: IO[String] =
    IO.fromOption(sys.env.get("GH_TOKEN"))(new RuntimeException("Could not get GitHub token from the environment"))

  private val gitHubAuthHeader: IO[Authorization] =
    token.map(t => Authorization(Credentials.Token(AuthScheme.Bearer, t)))

  def getRequest(uri: Uri): IO[Request[IO]] =
    gitHubAuthHeader.map(header => Request(uri = uri, headers = Headers(header)))

  def orgContributors(
    client: Client[IO],
    orgName: String,
    startPageNumber: Int,
    lastPageNumber: Int,
    perPage: Int
  ): IO[List[ContributorResponseDTO]] = {
    val orgRepoUrisIO =
      for {
        range <- IO.pure(startPageNumber to lastPageNumber)
        orgRepoUris <-
          range.map(pageNum => GitHubLinkGenerator.orgRepositories(orgName, perPage, pageNum)).toList.sequence
      } yield orgRepoUris

    import ClientModel.Instances._
    import cats.syntax.semigroup._
    Stream
      .evalSeq(orgRepoUrisIO)
      .mapAsyncUnordered(15) { uri =>
        IO.println("Fetching " + uri) >> client.expect[List[GitHubProtocol.Repository]](getRequest(uri))
      }
      .mapAsyncUnordered(15) { repos =>
        repos.map(repo => GitHubLinkGenerator.repositoryContributors(orgName, repo.name)).sequence
      }
      .mapAsyncUnordered(15) { uris =>
        uris
          .map(getRequest)
          .map(req =>
            client
              .expect[List[GitHubProtocol.Contributor]](req)
              .onError(er => req.flatMap(r => IO.println("Error from " + r.uri + " - " + er.getMessage)))
              .handleError(_ => Nil)
          )
          .sequence
          .map(_.reduce(_ combine _))
      }
      .compile
      .fold(List.empty[GitHubProtocol.Contributor])((agg, lc) => agg combine lc)
      .map(_.map(ContributorResponseDTO.fromContributor).sortBy(_.contributions)(Ordering.fromLessThan(_ > _)))
  }

}
