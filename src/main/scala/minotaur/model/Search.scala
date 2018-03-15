package minotaur.model

import scala.collection.mutable.ListBuffer

trait Search {
  def reconstructPath(current: SearchNode): Option[Path] = {
    val lb = ListBuffer.empty[Location]
    var pathItem = current

    do {
      lb += pathItem.location
      // when jumping, pathItem.parent can be empty
      // the getOrElse(pathItem) is cheating but deals with that
      pathItem = pathItem.parent.getOrElse(pathItem)
    } while (pathItem.parent.isDefined)

    // generously assume the path is shortest
    Some(ShortestPath(lb.toList.reverse))
  }

  def findPath(
    board: Board,
    from: Location,
    direction: Direction
  ): Option[Path]
}

trait SearchNode {
  def location: Location
  def parent: Option[SearchNode]
  def cost: Int
}


// TODO the below should be DI-ified
import minotaur.search._
object Search extends Search {
  def findPath(board: Board, from: Location, direction: Direction): Option[Path] =
    profile.Profiler.profile("BFS", BFS.findPath(board, from, direction))
}
