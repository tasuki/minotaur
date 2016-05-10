package minotaur

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
}

sealed trait WallOrientation
case object Horizontal extends WallOrientation
case object Vertical extends WallOrientation

case class Wall(
  topLeft: Location,
  orientation: WallOrientation,
  boardType: BoardType
) {
  require(
    boardType.wallLocations contains topLeft,
    "This wall is not permissible"
  )
}

case class BoardType(size: Int = 9) {
  // used to verify location validity
  val possibleLocations: Set[Int] = (
    for (field <- 0 to size * size - 1) yield field
  )(collection.breakOut)

  // all locations on the board
  val locations: Set[Location] =
    for (field <- possibleLocations)
      yield Location(field, this)

  // used to verify wall validity
  val wallLocations: Set[Location] = for {
    location <- locations
    if (!location.isSouthBorder) && (!location.isEastBorder)
  } yield location

  // possible walls on the board
  val possibleWalls: Set[Wall] = for {
    location <- wallLocations
    orientation <- List(Horizontal, Vertical)
  } yield Wall(location, orientation, this)
}

case class Board(
  boardType: BoardType,
  black: Location,
  white: Location,
  walls: Set[Wall] = Set(),
  size: Int = 9
) {
}

case class GameState(
  board: Board,
  blackWalls: Int,
  whiteWalls: Int,
  whiteOnTurn: Boolean
)
