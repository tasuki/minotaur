package minotaur.model

import scala.collection.mutable.ListBuffer

trait Search {
  def reconstructPath(current: SearchNode): Option[Seq[Location]] = {
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
  ): Option[Seq[Location]]
}

trait SearchNode {
  def location: Location
  def parent: Option[SearchNode]
  def cost: Int
}


// TODO the below should be DI-ified
import minotaur.search._
object Search extends Search {
  def findPath(board: Board, from: Location, direction: Direction) =
    AStar.findPath(board, from, direction)
}
