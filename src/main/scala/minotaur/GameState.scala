package minotaur

case class Location(location: Int, boardType: BoardType) {
  val size = boardType.size

  def isNorthBorder() =
    location < size

  def isSouthBorder() =
    location >= size * (size - 1)

  def isEastBorder() =
    location % size == size - 1

  def isWestBorder() =
    location % size == 0
}


sealed trait WallOrientation
case object Horizontal extends WallOrientation
case object Vertical extends WallOrientation

case class Wall(topLeft: Location, orientation: WallOrientation)


case class BoardType(size: Int = 9) {
  val fields: Seq[Location] = for(item <- 0 to size*size-1) yield (Location(item, this))

  val possibleWalls: Set[Wall] = (for {
    field <- fields if (! field.isSouthBorder) && (! field.isEastBorder)
    orientation <- List(Horizontal, Vertical)
  } yield Wall(field, orientation))(collection.breakOut)
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
