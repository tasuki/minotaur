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

  def borders(direction: Direction): Boolean = direction match {
    case d @ (North | West) => location.isBorder(d)
    case d @ (South | East) => location.neighbor(d).map(_.isBorder(d)).get
  }

  private def wallOverlaps: List[Wall] = {
    var mutableList = collection.mutable.MutableList[Wall]()
    if (orientation == Vertical) {
      if (!borders(North)) {
        mutableList += Wall(location.neighbor(North).get, orientation)
      }
      if (!borders(South)) {
        mutableList += Wall(location.neighbor(South).get, orientation)
      }
    } else {
      if (!borders(East)) {
        mutableList += Wall(location.neighbor(East).get, orientation)
      }
      if (!borders(West)) {
        mutableList += Wall(location.neighbor(West).get, orientation)
      }
    }
    mutableList.toList
  }

  def blocks: List[Wall] = Wall(location, orientation.opposite) :: wallOverlaps
}
