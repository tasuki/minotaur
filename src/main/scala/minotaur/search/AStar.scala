package minotaur.search

import scala.collection.mutable.{ArraySeq,ListBuffer}
import minotaur.model.{Search,SearchNode,Board,Location,Direction}

object AStar extends Search {
  case class Node(
    location: Location,
    parent: Option[Node],
    cost: Int,
    estimatedDistance: Int
  ) extends SearchNode {
    val priority =
      // adding a small fraction to make these unique by location
      estimatedDistance + cost + location.location / 1000.0
  }

  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Seq[Location]] = {
    val closed: ArraySeq[Boolean] =
      ArraySeq.fill(board.boardType.size * board.boardType.size){false}

    var open = ListBuffer[Node]()
    open += Node(from, None, 0, from.estimateDistance(direction))

    while (open.nonEmpty) {
      // best score from the open nodes
      open = open.sortBy(n => n.priority)
      val current = open.remove(0)

      closed(current.location.location) = true

      if (current.location.isBorder(direction))
        return reconstructPath(current)

      // loop through current neighbors
      for (neighbor <- board.neighbors(current.location)) {
        // only non-closed nodes
        if (closed(neighbor.location) == false) {
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

    return None
  }
}
