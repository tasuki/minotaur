package minotaur.model

trait Search {
  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Seq[Location]]
}

trait SearchNode {
  def location: Location
  def parent: Option[SearchNode]
  def cost: Int
}

import minotaur.search._

object Search extends Search {
  def findPath(board: Board, from: Location, direction: Direction) =
    AStar.findPath(board, from, direction)
}
