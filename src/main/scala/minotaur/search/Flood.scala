package minotaur.search

import scala.collection.mutable
import scala.collection.mutable.{ ArraySeq, Queue }

import minotaur.model.{ Board, Location, SearchNode }

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
    val open: mutable.Queue[Node] = mutable.Queue[Node]()
    val closed: mutable.ArraySeq[Option[Node]] =
      mutable.ArraySeq.fill(board.boardType.size * board.boardType.size){None}

    val startingNode = Node(from, None, 0)
    open += startingNode
    closed(from.location) = Some(startingNode)

    while (open.nonEmpty) {
      val current = open.dequeue

      for (neighbor <- board.neighbors(current.location)) {
        if (closed(neighbor.location).isEmpty) {
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

    closed.flatten.toSet
  }
}
