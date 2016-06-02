package minotaur.model

case class Location(location: Int, boardType: BoardType) {
  require(
    boardType.containsLocation(location),
    "Location not on board"
  )

  private val boardSize = boardType.size

  def isBorder(direction: Direction): Boolean =
    estimateDistance(direction) == 0

  def neighbor(direction: Direction): Option[Location] =
    if (isBorder(direction)) None
    else direction match {
      case North => Some(boardType.locations(location - boardSize))
      case South => Some(boardType.locations(location + boardSize))
      case East => Some(boardType.locations(location + 1))
      case West => Some(boardType.locations(location - 1))
    }

  lazy val allowsWallPlacement: Boolean =
    boardType.possibleWallLocations contains this

  private val distances: Map[Direction, Int] = Map(
    North -> (location / boardSize),
    South -> (boardSize - 1 - location / boardSize),
    East -> (boardSize - 1 - location % boardSize),
    West -> (location % boardSize)
  )

  def estimateDistance(direction: Direction): Int = distances(direction)
}
