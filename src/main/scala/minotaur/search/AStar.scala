package minotaur.search

import scala.collection.mutable
import scala.collection.mutable.{ ArraySeq, ListBuffer }

import minotaur.model._

object AStar extends Search {
  case class Node(
    location: Location,
    parent: Option[Node],
    cost: Int,
    estimatedDistance: Int
  ) extends SearchNode {
    val priority: Double =
      // adding a small fraction to make these unique by location
      estimatedDistance + cost + location.location / 1000.0
  }

  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Path] = {
    val closed: mutable.ArraySeq[Boolean] =
      mutable.ArraySeq.fill(board.boardType.size * board.boardType.size){false}

    var open = ListBuffer[Node]()
    open += Node(from, None, 0, from.estimateDistance(direction))

    while (open.nonEmpty) {
      // best score from the open nodes
      val current = open.minBy(_.priority)
      open -= current

      closed(current.location.location) = true

      if (current.location.isBorder(direction))
        return reconstructPath(current)

      // loop through current neighbors
      for (neighbor <- board.neighbors(current.location)) {
        // only non-closed nodes
        if (!closed(neighbor.location)) {
          val newNode = Node(
            neighbor,
            Some(current),
            current.cost + 1,
            neighbor.estimateDistance(direction)
          )

          open.find(_.location == neighbor) match {
            case Some(node) =>
              if (newNode.cost < node.cost) {
                // node is open and we can reach it in a better way
                open -= node
                open += newNode
              }
            case None =>
              // node isn't open yet, add it
              open += newNode
          }
        }
      }
    }

    None
  }
}
