package minotaur.search

import scala.collection.mutable.{SortedSet,Set}
import minotaur.model.{Search,SearchNode,Board,Location,Direction}

object AStar extends Search {
  case class Node(
    location: Location,
    parent: Option[Node],
    cost: Int,
    estimatedDistance: Int
  ) extends SearchNode

  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Seq[Location]] = {

    val closed = Set[Node]()
    // SortedSet has log head lookup, but also log find/add/remove
    // TODO Investigate Set: linear head lookup, but constant find/add/remove
    val open = SortedSet.empty(Ordering.by((n: Node) =>
      // adding a small fraction to make these unique by location
      n.estimatedDistance + n.cost + (n.location.location / 1000.0)
    ))
    open += Node(from, None, 0, from.estimateDistance(direction))

    while (open.nonEmpty) {
      // best score from the open nodes
      val current = open.head
      open.remove(current)
      closed.add(current)

      // for debugging
      if (false) {
        println(minotaur.io.BoardPrinter.printSearchNodes(board, closed.toSet))
      }

      if (current.location.isBorder(direction))
        return reconstructPath(current)

      // loop through current neighbors
      for (neighbor <- board.neighbors(current.location)) {
        // only non-closed nodes
        if (closed.find(_.location == neighbor).isEmpty) {
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
