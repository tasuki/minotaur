package minotaur.model

import scala.annotation.tailrec

sealed trait Path {
  val path: Seq[Location]
  lazy val potentialize: Path = PotentialShortestPath(path)
  lazy val validate: Path = ShortestPath(path)
  lazy val forward: Path = ShortestPath(path.tail)

  def startsWith(location: Location): Boolean =
    path.head == location

  def isValid(player: Player, board: Board): Boolean = {
    @tailrec def check(loc: Location, path: Seq[Location]): Boolean = {
      if (!board.canMove(loc, path.head)) false
      else if (path.tail.isEmpty) true
      else check(path.head, path.tail)
    }

    check(board.pawnLocation(player), path)
  }
}

case class ShortestPath(path: Seq[Location]) extends Path
case class PotentialShortestPath(path: Seq[Location]) extends Path
