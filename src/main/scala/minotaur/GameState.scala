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

sealed trait WallOrientation {
  val opposite = this match {
    case Horizontal => Vertical
    case Vertical => Horizontal
  }
}
case object Horizontal extends WallOrientation
case object Vertical extends WallOrientation

case class Wall(
  topLeft: Location,
  orientation: WallOrientation
) {
  require(
    topLeft.boardType.possibleWallLocations contains topLeft,
    "This wall is not permissible"
  )

  val isNorthmost = topLeft.isNorthBorder
  val isSouthmost = topLeft.southernNeighbor.map(_.isSouthBorder).getOrElse(true)
  val isEastmost = topLeft.easternNeighbor.map(_.isEastBorder).getOrElse(true)
  val isWestmost = topLeft.isWestBorder
}

case class BoardType(size: Int = 9) {
  // used to verify location validity
  val possibleLocations: Vector[Int] = (
    for (field <- 0 to size * size - 1) yield field
  )(collection.breakOut)

  // all locations on the board for jumping around
  val locations: Vector[Location] =
    for (field <- possibleLocations)
      yield Location(field, this)

  // used to verify wall validity
  val possibleWallLocations: Vector[Location] = for {
    location <- locations
    if (!location.isSouthBorder) && (!location.isEastBorder)
  } yield location

  // possible walls on the board
  val possibleWalls: Vector[Wall] = for {
    location <- possibleWallLocations
    orientation <- List(Horizontal, Vertical)
  } yield Wall(location, orientation)
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
