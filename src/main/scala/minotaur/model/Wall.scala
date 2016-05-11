package minotaur.model

sealed trait WallOrientation {
  val opposite = this match {
    case Vertical => Horizontal
    case Horizontal => Vertical
  }
  val directions = this match {
    case Vertical => Seq(North, South)
    case Horizontal => Seq(East, West)
  }
}
case object Vertical extends WallOrientation
case object Horizontal extends WallOrientation

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

  private def extensionOverlaps: Seq[Wall] = for {
    direction <- orientation.directions
    loc <- location.neighbor(direction) if !borders(direction)
  } yield Wall(loc, orientation)

  def overlaps: Seq[Wall] = extensionOverlaps :+ Wall(location, orientation.opposite)
}
