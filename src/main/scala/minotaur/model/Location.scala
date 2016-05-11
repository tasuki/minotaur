package minotaur.model

case class Location(location: Int, boardType: BoardType) {
  require(
    boardType.possibleLocations contains location,
    "Location not on board"
  )

  private val boardSize = boardType.size
  private val locations = boardType.locations

  def isBorder(direction: Direction): Boolean = direction match {
    case North => location < boardSize
    case South => location >= boardSize * (boardSize - 1)
    case East => location % boardSize == boardSize - 1
    case West => location % boardSize == 0
  }

  def neighbor(direction: Direction): Option[Location] =
    if (isBorder(direction)) None
    else direction match {
      case North => Some(locations(location - boardSize))
      case South => Some(locations(location + boardSize))
      case East => Some(locations(location + 1))
      case West => Some(locations(location - 1))
    }
}
