package minotaur.model

case class Location(location: Int, boardType: BoardType) {
  require(
    boardType.possibleLocations contains location,
    "Location not on board"
  )

  private val boardSize = boardType.size

  val isNorthBorder = location < boardSize
  val isSouthBorder = location >= boardSize * (boardSize - 1)
  val isEastBorder = location % boardSize == boardSize - 1
  val isWestBorder = location % boardSize == 0

  def northernNeighbor: Option[Location] =
    if (isNorthBorder) None
    else Some(boardType.locations(location - boardSize))

  def southernNeighbor: Option[Location] =
    if (isSouthBorder) None
    else Some(boardType.locations(location + boardSize))

  def easternNeighbor: Option[Location] =
    if (isEastBorder) None
    else Some(boardType.locations(location + 1))

  def westernNeighbor: Option[Location] =
    if (isWestBorder) None
    else Some(boardType.locations(location - 1))
}
