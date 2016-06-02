package minotaur.search

import scala.collection.mutable.{SortedSet,Set,ListBuffer}
import minotaur.model.{Board,Location}
import minotaur.model.{Direction,North,South}

case class Node(
  location: Location,
  parent: Option[Node],
  cost: Int,
  estimatedDistance: Int
) {
  val heuristic = cost + estimatedDistance

  override def toString =
    s"(${location.location}: $cost, $estimatedDistance)"
}

object NodeOrdering extends Ordering[Node] {
  def compare(a: Node, b: Node): Int =
    if (a.location == b.location) 0
    else if (a.heuristic < b.heuristic) -1
    else if (a.heuristic > b.heuristic) 1
    else if (a.location.location < b.location.location) -1
    else 1
}

object AStar {
  private def reconstructPath(current: Node) = {
    val lb = ListBuffer.empty[Location]
    var pathItem = current

    do {
      lb += pathItem.location
      pathItem = pathItem.parent.get
    } while (pathItem.parent.isDefined)

    Some(lb.toList.reverse)
  }

  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Seq[Location]] = {

    val closed = Set[Node]()
    // SortedSet has log head lookup, but also log find/add/remove
    // TODO Investigate Set: linear head lookup, but constant find/add/remove
    val open = SortedSet.empty(NodeOrdering)
    open += Node(from, None, 0, from.estimateDistance(direction))

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
