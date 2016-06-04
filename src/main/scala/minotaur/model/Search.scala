package minotaur.model

trait Search {
  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Seq[Location]]
}

import minotaur.search.AStar

object Search extends Search {
  def findPath(board: Board, from: Location, direction: Direction) =
    AStar.findPath(board, from, direction)
}
