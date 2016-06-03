package minotaur.model

sealed trait Orientation {
  val opposite = this match {
    case Vertical => Horizontal
    case Horizontal => Vertical
  }
  val directions = this match {
    case Vertical => Seq(North, South)
    case Horizontal => Seq(East, West)
  }
}
case object Vertical extends Orientation
case object Horizontal extends Orientation

case class Wall(
  location: Location,
  orientation: Orientation
) {
  require(
    location.allowsWallPlacement,
    "This wall is not permissible"
  )

  override def toString = f"(Wall: ${location.location}%2d $orientation)"

  def borders(direction: Direction): Boolean = direction match {
    case d @ (North | West) => location.isBorder(d)
    case d @ (South | East) => location.neighbor(d).map(_.isBorder(d)).get
  }

  private def extensionOverlaps: Seq[Wall] = for {
    direction <- orientation.directions
    loc <- location.neighbor(direction) if !borders(direction)
  } yield Wall(loc, orientation)

  lazy val overlaps: Seq[Wall] = extensionOverlaps :+ Wall(location, orientation.opposite)

  private def touchesI = for {
    direction <- orientation.directions
    extension <- location.neighbor(direction).flatMap(_.neighbor(direction))
      if extension.allowsWallPlacement
  } yield Wall(extension, orientation)

  private def touchesL = for {
    dir1 <- orientation.directions
    dir2 <- orientation.opposite.directions
    extension <- location.neighbor(dir1).flatMap(_.neighbor(dir2))
      if extension.allowsWallPlacement
  } yield Wall(extension, orientation.opposite)

  private def touchesT = for {
    dir <- Direction.all
    extension <- location.neighbor(dir)
      if extension.allowsWallPlacement
  } yield Wall(extension, orientation.opposite)

  lazy val touches: Seq[Wall] = touchesI ++ touchesL ++ touchesT
}
