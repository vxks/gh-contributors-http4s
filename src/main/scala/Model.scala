import cats.Semigroup

object ClientModel {

  sealed trait GitHubProtocol

  object GitHubProtocol {
    case class Contributor(id: Long, login: String, contributions: Int) extends GitHubProtocol
    case class Repository(id: Long, name: String)                       extends GitHubProtocol
  }

  object Instances {
    implicit val contributorsSemigroup: Semigroup[List[GitHubProtocol.Contributor]] =
      (x: List[GitHubProtocol.Contributor], y: List[GitHubProtocol.Contributor]) =>
        (x ++ y)
          .groupBy(c => (c.id, c.login))
          .map { case (id, login) -> cs =>
            val newContributions = cs.map(_.contributions).sum
            GitHubProtocol.Contributor(id, login, newContributions)
          }
          .toList
  }
}

object ServerModel {
  case class ContributorResponseDTO(login: String, contributions: Int)

  object ContributorResponseDTO {
    import ClientModel._
    def fromContributor(contributor: GitHubProtocol.Contributor): ContributorResponseDTO =
      ContributorResponseDTO(contributor.login, contributor.contributions)
  }
}
