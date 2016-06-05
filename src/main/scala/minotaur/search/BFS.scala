package minotaur.search

import scala.collection.mutable.{ArraySeq,Queue}
import minotaur.model.{Search,SearchNode,Board,Location,Direction,Path}

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
    val closed: ArraySeq[Boolean] =
      ArraySeq.fill(board.boardType.size * board.boardType.size){false}

    val open: Queue[Node] = Queue[Node]()
    open += Node(from, None, 0)

    while (open.nonEmpty) {
      val current = open.dequeue
      closed(current.location.location) = true

      if (current.location.isBorder(direction))
        return reconstructPath(current)

      for (neighbor <- board.neighbors(current.location)) {
        if (closed(neighbor.location) == false) {
          open += Node(
            neighbor,
            Some(current),
            current.cost + 1
          )
        }
      }
    }

    None
  }
}
