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

  private def wallOverlaps: Seq[Wall] = {
    if (orientation == Vertical)
      for {
        direction <- Seq(North, South)
        loc <- location.neighbor(direction) if !borders(direction)
      } yield Wall(loc, orientation)
    else
      for {
        direction <- Seq(East, West)
        loc <- location.neighbor(direction) if !borders(direction)
      } yield Wall(loc, orientation)
  }

  def blocks: Seq[Wall] = wallOverlaps :+ Wall(location, orientation.opposite)
}
