package profile

import minotaur.io.BoardReader
import minotaur.model._
import minotaur.search.{ AStar, BFS }

object Benchmark {
  val bt = BoardType()

  def main(args: Array[String]): Unit = {
    profileBoardWithLocations()
    profileNeighbors()
    profileContains()
    mapCaseClassVsListId()
    searches()
  }

  def profileBoardWithLocations(): Unit = {
    val locationList: List[Location] = (for(i <- 0 to 80) yield Location(i, bt)).toList
    val locationVector: Vector[Location] = locationList.toVector

    Profiler.clear()
    Profiler.profileMany("List access (2)", locationList(2))
    Profiler.profileMany("List access (73)", locationList(73))

    Profiler.profileMany("Vector access (2)", locationVector(2))
    Profiler.profileMany("Vector access (73)", locationVector(73)) // this is bad
    Profiler.printShort()
  }

  def profileNeighbors(): Unit = {
    val neighborVector: Vector[Seq[Direction]] =
      bt.locations.map(_ => Direction.all)
    val neighborList: List[Seq[Direction]] =
      neighborVector.toList
    val neighborMap: Map[Location, Seq[Direction]] =
      bt.locations.map(location => location -> Direction.all).toMap

    val two = Location(2, bt)
    val many = Location(73, bt)

    Profiler.clear()
    Profiler.profileMany("Neighbor vector (73)", neighborVector(many.location))
    Profiler.profileMany("Neighbor vector (2)", neighborVector(two.location))
    Profiler.profileMany("Neighbor list (73)", neighborList(many.location))
    Profiler.profileMany("Neighbor list (2)", neighborList(two.location))
    Profiler.profileMany("Neighbor map (73)", neighborMap(many))
    Profiler.profileMany("Neighbor map (2)", neighborMap(two))
    Profiler.printShort()
  }

  def profileContains(): Unit = {
    val wallSet: Set[Wall] = bt.possibleWalls
    val wallList: List[Wall] = wallSet.toList.sortBy(_.location.location)
    val wallVector: Vector[Wall] = wallList.toVector
    val wall = Wall(Location(40, bt), Horizontal)

    Profiler.clear()
    Profiler.profileMany("Set contains", wallSet contains wall)
    Profiler.profileMany("List contains", wallList contains wall) // not so bad
    Profiler.profileMany("Vector contains", wallVector contains wall) // this is bad
    Profiler.printShort()
  }

  def mapCaseClassVsListId(): Unit = {
    val mapCaseClass = Map(
      North -> Location(31, bt),
      South -> Location(49, bt),
      East -> Location(41, bt),
      West -> Location(39, bt)
    )
    val listInts = List(
      Location(31, bt),
      Location(49, bt),
      Location(41, bt),
      Location(39, bt)
    )
    val vectorInts = Vector(
      Location(31, bt),
      Location(49, bt),
      Location(41, bt),
      Location(39, bt)
    )

    Profiler.clear()
    Profiler.profileMany("Directions case class", {
      mapCaseClass(South)
      mapCaseClass(West)
    }) // about 4 times slower that list/vector
    Profiler.profileMany("Directions list ints", {
      listInts(1)
      listInts(3)
    })
    Profiler.profileMany("Directions vector ints", {
      vectorInts(1)
      vectorInts(3)
    })
    Profiler.printShort()
  }

  def searches(): Unit = {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)

    Profiler.clear()
    Map("A*" -> AStar, "BFS" -> BFS).foreach {
      case (name, algo) =>
        Profiler.profileMany(name, 10000, {
          algo.findPath(board, board.pawnLocation(White), South)
          algo.findPath(board, board.pawnLocation(Black), North)
        })
    }
    Profiler.printShort()
  }
}
