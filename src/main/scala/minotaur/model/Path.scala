package minotaur.model

sealed trait Path {
  val path: Seq[Location]
  lazy val potentialize: Path = PotentialShortestPath(path)
  lazy val advance: Path = ShortestPath(path.tail)

  def startsWith(location: Location): Boolean =
    path.head == location
}

case class ShortestPath(path: Seq[Location]) extends Path
case class PotentialShortestPath(path: Seq[Location]) extends Path
