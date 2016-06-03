package minotaur.model

case class Location(location: Int, boardType: BoardType) {
  require(
    boardType.containsLocation(location),
    "Location not on board"
  )

  private val boardSize = boardType.size

  lazy val allowsWallPlacement: Boolean =
    boardType.possibleWallLocations contains this

  val estimateDistance: Map[Direction, Int] = Map(
    North -> (location / boardSize),
    South -> (boardSize - 1 - location / boardSize),
    East -> (boardSize - 1 - location % boardSize),
    West -> (location % boardSize)
  )

  val isBorder: Map[Direction, Boolean] =
    Direction.all.map(dir => dir -> (estimateDistance(dir) == 0)).toMap

  lazy val neighbor: Map[Direction, Option[Location]] =
    Direction.all.map(dir => dir -> (
      if (isBorder(dir)) None
      else dir match {
        case North => Some(boardType.locations(location - boardSize))
        case South => Some(boardType.locations(location + boardSize))
        case East => Some(boardType.locations(location + 1))
        case West => Some(boardType.locations(location - 1))
      }
    )).toMap
}
