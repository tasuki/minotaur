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
  location: Location,
  orientation: WallOrientation
) {
  require(
    location.boardType.possibleWallLocations contains location,
    "This wall is not permissible"
  )

  val isNorthmost = location.isNorthBorder
  val isSouthmost = location.southernNeighbor.map(_.isSouthBorder).getOrElse(true)
  val isEastmost = location.easternNeighbor.map(_.isEastBorder).getOrElse(true)
  val isWestmost = location.isWestBorder

  private def wallOverlaps: List[Wall] = {
    var mutableList = collection.mutable.MutableList[Wall]()
    if (orientation == Vertical) {
      if (!isNorthmost) {
        mutableList += Wall(location.northernNeighbor.get, orientation)
      }
      if (!isSouthmost) {
        mutableList += Wall(location.southernNeighbor.get, orientation)
      }
    } else {
      if (!isEastmost) {
        mutableList += Wall(location.easternNeighbor.get, orientation)
      }
      if (!isWestmost) {
        mutableList += Wall(location.westernNeighbor.get, orientation)
      }
    }
    mutableList.toList
  }

  def blocks: List[Wall] = Wall(location, orientation.opposite) :: wallOverlaps
}
