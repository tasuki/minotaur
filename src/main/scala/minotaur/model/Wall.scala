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
  val allDirections = Seq(North, South, East, West)
}
case object Vertical extends WallOrientation
case object Horizontal extends WallOrientation

case class Wall(
  location: Location,
  orientation: WallOrientation
) {
  require(
    location.allowsWallPlacement,
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

  private def touchesI = for {
    direction <- orientation.directions
    extension <- location.neighbor(direction).flatMap(l => l.neighbor(direction))
      if extension.allowsWallPlacement
  } yield Wall(extension, orientation)

  private def touchesL = for {
    dir1 <- orientation.directions
    dir2 <- orientation.opposite.directions
    extension <- location.neighbor(dir1).flatMap(l => l.neighbor(dir2))
      if extension.allowsWallPlacement
  } yield Wall(extension, orientation.opposite)

  private def touchesT = for {
    dir <- orientation.allDirections
    extension <- location.neighbor(dir)
      if extension.allowsWallPlacement
  } yield Wall(extension, orientation.opposite)

  def touches: Seq[Wall] = touchesI ++ touchesL ++ touchesT
}
