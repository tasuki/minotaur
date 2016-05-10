package minotaur

case class Location(location: Int, boardType: BoardType) {
  require(boardType.possibleFields contains location, "Location not on board...")

  private val boardSize = boardType.size

  val isNorthBorder = location < boardSize
  val isSouthBorder = location >= boardSize * (boardSize - 1)
  val isEastBorder = location % boardSize == boardSize - 1
  val isWestBorder = location % boardSize == 0
}


sealed trait WallOrientation
case object Horizontal extends WallOrientation
case object Vertical extends WallOrientation

case class Wall(topLeft: Location, orientation: WallOrientation)


case class BoardType(size: Int = 9) {
  val possibleFields: Set[Int] = (
    for(field <- 0 to size * size - 1) yield field
  )(collection.breakOut)

  val fields: Set[Location] =
    for(field <- possibleFields)
    yield Location(field, this)

  val possibleWalls: Set[Wall] = for {
    field <- fields if (! field.isSouthBorder) && (! field.isEastBorder)
    orientation <- List(Horizontal, Vertical)
  } yield Wall(field, orientation)
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
