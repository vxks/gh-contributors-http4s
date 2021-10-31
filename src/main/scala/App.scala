import cats.effect.{ExitCode, IO, IOApp, Resource}
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.circe._
import org.http4s.client._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Server
import org.http4s.{Header, HttpRoutes, Response, Uri, _}
import org.typelevel.ci.CIString

object App extends IOApp {

  def extractLastLink(headers: List[Header.Raw]): IO[Uri] =
    IO.fromOption(headers.find(_.name == CIString("link")))(
      new Exception("Could not extract `link` header from GitHub response")
    ).flatMap { rawHeader =>
      /*
        Header format:
        ```link: <https://api.github.com/organizations/1527492/repos?per_page=2&page=2>; rel="next",
        <https://api.github.com/organizations/1527492/repos?per_page=2&page=13>; rel="last"```
       */
      val pattern = """.*<(.*)>; rel="last".*""".r
      val urlE = rawHeader.value match {
        case pattern(linkLast) => Right(linkLast)
        case _                 => Left(new Exception("Could not extract last page link from link header"))
      }
      val uriE = urlE.flatMap(Uri.fromString)
      IO.fromEither(uriE)
    }

  val itemsPerPage = 20

  type Route[F[_]] = PartialFunction[Request[F], F[Response[F]]]

  val contributorsRoute: Route[IO] = { case GET -> Root / "org" / orgName / "contributors" =>
    for {
      uri <- GitHubLinkGenerator.orgRepositories(orgName, itemsPerPage, 1)
      lastLink <- client.use(_.get(uri) { case Response(_, _, headers, _, _) =>
                    extractLastLink(headers.headers)
                  })

      lastPageNum <- IO.fromOption(lastLink.query.params.get("page").map(_.toInt))(
                       new RuntimeException("Could not extract last page number from response `link` header")
                     )

      contributors <- client.use(GitHubClient.orgContributors(_, orgName, 1, lastPageNum, itemsPerPage))
      response     <- Ok(contributors.asJson)
    } yield response
  }

  val mainService: HttpApp[IO] =
    HttpRoutes
      .of[IO](contributorsRoute)
      .orNotFound

  val server: Resource[IO, Server] = BlazeServerBuilder[IO]
    .bindHttp(8080, "localhost")
    .withHttpApp(mainService)
    .resource

  val client: Resource[IO, Client[IO]] =
    BlazeClientBuilder[IO].resource.onFinalize(IO.println("Shutting down client..."))

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _           <- IO.println("Starting server...")
      serverFiber <- server.useForever.start.onError(ex => IO.println(ex.getMessage))
      _           <- IO.println("Press ENTER to shut down server")
      _           <- IO.readLine
      _           <- IO.println("Shutting down server...") >> serverFiber.cancel
    } yield ExitCode.Success

}
