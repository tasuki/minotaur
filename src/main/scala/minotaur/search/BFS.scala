package minotaur.search

import scala.collection.mutable
import scala.collection.mutable.{ ArraySeq, Queue }

import minotaur.model._

object BFS extends Search {
  case class Node(
    location: Location,
    parent: Option[Node],
    cost: Int
  ) extends SearchNode

  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Path] = {
    val open: mutable.Queue[Node] = mutable.Queue[Node]()
    val closed: mutable.ArraySeq[Boolean] =
      mutable.ArraySeq.fill(board.boardType.size * board.boardType.size){false}

    open += Node(from, None, 0)
    closed(from.location) = true

    while (open.nonEmpty) {
      val current = open.dequeue

      if (current.location.isBorder(direction))
        return reconstructPath(current)

      for (neighbor <- board.neighbors(current.location)) {
        if (!closed(neighbor.location)) {
          open += Node(
            neighbor,
            Some(current),
            current.cost + 1
          )

          closed(neighbor.location) = true
        }
      }
    }

    None
  }
}
