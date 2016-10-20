package minotaur.search

import scala.collection.mutable.{ArraySeq,Queue}
import minotaur.model.{SearchNode,Board,Location}

object Flood {
  case class Node(
    location: Location,
    parent: Option[Node],
    cost: Int
  ) extends SearchNode

  def findNodes(
    board: Board,
    from: Location
  ): Set[Node] = {
    val open: Queue[Node] = Queue[Node]()
    val closed: ArraySeq[Option[Node]] =
      ArraySeq.fill(board.boardType.size * board.boardType.size){None}

    val startingNode = Node(from, None, 0)
    open += startingNode
    closed(from.location) = Some(startingNode)

    while (open.nonEmpty) {
      val current = open.dequeue

      for (neighbor <- board.neighbors(current.location)) {
        if (closed(neighbor.location) == None) {
          val node = Node(
            neighbor,
            Some(current),
            current.cost + 1
          )

          open += node
          closed(neighbor.location) = Some(node)
        }
      }
    }

    return closed.flatten.toSet
  }
}
