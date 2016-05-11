package minotaur.model

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
