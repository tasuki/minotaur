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

  val isNorthmost = location.isBorder(North)
  val isSouthmost = location.neighbor(South).map(_.isBorder(South)).getOrElse(true)
  val isEastmost = location.neighbor(East).map(_.isBorder(East)).getOrElse(true)
  val isWestmost = location.isBorder(West)

  private def wallOverlaps: List[Wall] = {
    var mutableList = collection.mutable.MutableList[Wall]()
    if (orientation == Vertical) {
      if (!isNorthmost) {
        mutableList += Wall(location.neighbor(North).get, orientation)
      }
      if (!isSouthmost) {
        mutableList += Wall(location.neighbor(South).get, orientation)
      }
    } else {
      if (!isEastmost) {
        mutableList += Wall(location.neighbor(East).get, orientation)
      }
      if (!isWestmost) {
        mutableList += Wall(location.neighbor(West).get, orientation)
      }
    }
    mutableList.toList
  }

  def blocks: List[Wall] = Wall(location, orientation.opposite) :: wallOverlaps
}
