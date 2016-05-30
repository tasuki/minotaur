package minotaur.search

import scala.collection.mutable.{SortedSet,Set,ListBuffer}
import minotaur.model.{Board,Location}
import minotaur.model.{Direction,North,South}

case class Node(
  location: Location,
  parent: Option[Node],
  costSoFar: Int,
  estimatedDistance: Int
)

object AStar {
  private def estimateDistance(
    board: Board,
    from: Location,
    direction: Direction
  ): Int = {
    direction match {
      case North => from.location / board.size
      case South => board.size - from.location / board.size
      case _ => throw new IllegalArgumentException
    }
  }

  private def reconstructPath(current: Node) = {
    var lb = ListBuffer.empty[Location]
    var pathItem = current

    do {
      lb += pathItem.location
      pathItem = pathItem.parent.get
    } while (pathItem.parent.isDefined)

    Some(lb.toList.reverse)
  }

  def findPath(board: Board, from: Location, direction: Direction): Option[Seq[Location]] = {
    var closed = Set[Node]()
    var open = SortedSet.empty(Ordering.by((n: Node) => {
      // adding a small fraction to make these unique by location
      n.estimatedDistance + n.costSoFar + (n.location.location / 1000.0)
    }))
    open += Node(from, None, 0, estimateDistance(board, from, direction))

    while (open.nonEmpty) {
      // best score from the open nodes
      val current = open.head
      open.remove(current)
      closed.add(current)

      // for debugging
      if (false) {
        println(minotaur.io.BoardPrinter.printSearchNodes(board, closed))
      }

      if (current.location.isBorder(direction))
        return reconstructPath(current)

      // loop through current neighbors
      for (neighbor <- board.neighbors(current.location)) {
        // only non-closed nodes
        if (closed.find(_.location == neighbor).isEmpty) {
          val potentialCost = current.costSoFar + 1

          val node: Option[Node] = open.find(_.location == neighbor)
          if (node.isDefined) {
            // node is open, see if we can reach it in a better way
            if (potentialCost < node.get.costSoFar) {
              open -= node.get
              open += Node(
                neighbor,
                Some(current),
                potentialCost,
                node.get.estimatedDistance
              )
            }
          } else {
            // node isn't open yet, add it
            open += Node(
              neighbor,
              Some(current),
              potentialCost,
              estimateDistance(board, neighbor, direction)
            )
          }
        }
      }
    }

    return None
  }
}
