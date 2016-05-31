package minotaur.model

case class Location(location: Int, boardType: BoardType) {
  require(
    boardType.containsLocation(location),
    "Location not on board"
  )

  private val boardSize = boardType.size

  def isBorder(direction: Direction): Boolean = direction match {
    case North => location < boardSize
    case South => location >= boardSize * (boardSize - 1)
    case East => location % boardSize == boardSize - 1
    case West => location % boardSize == 0
  }

  def neighbor(direction: Direction): Option[Location] =
    if (isBorder(direction)) None
    else direction match {
      case North => Some(boardType.locations(location - boardSize))
      case South => Some(boardType.locations(location + boardSize))
      case East => Some(boardType.locations(location + 1))
      case West => Some(boardType.locations(location - 1))
    }

  def allowsWallPlacement: Boolean =
    boardType.possibleWallLocations contains this

  def estimateDistance(direction: Direction): Int =
    direction match {
      case North => location / boardSize
      case South => boardSize - 1 - location / boardSize
      case East => boardSize - 1 - location % boardSize
      case West => location % boardSize
    }
}
