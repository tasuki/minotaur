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

  lazy val borders: Map[Direction, Boolean] =
    Direction.all.map(dir => dir -> (dir match {
      case d @ (North | West) => location.isBorder(d)
      case d @ (South | East) => location.neighbor(d).map(_.isBorder(d)).get
    })).toMap

  lazy val overlaps: Seq[Wall] = {
    def extensionOverlaps: Seq[Wall] = for {
      direction <- orientation.directions
      loc <- location.neighbor(direction) if !borders(direction)
    } yield loc.walls(orientation)

    extensionOverlaps :+ location.walls(orientation.opposite)
  }

  lazy val blocksMovement: Seq[(Location, Direction)] = {
    // the neighbors exist, otherwise the wall would be illegal
    // NW, NE, SW, SE
    val locations = Seq(
      location,
      location.neighbor(East).get,
      location.neighbor(South).get,
      location.neighbor(South).flatMap(_.neighbor(East)).get
    )

    val directions = orientation match {
      case Horizontal => Seq(South, South, North, North)
      case Vertical => Seq(East, West, East, West)
    }

    (locations zip directions)
  }

  lazy val touches: Seq[Wall] = {
    def touchesI = for {
      direction <- orientation.directions
      extension <- location.neighbor(direction).flatMap(_.neighbor(direction))
        if extension.allowsWallPlacement
    } yield extension.walls(orientation)

    def touchesL = for {
      dir1 <- orientation.directions
      dir2 <- orientation.opposite.directions
      extension <- location.neighbor(dir1).flatMap(_.neighbor(dir2))
        if extension.allowsWallPlacement
    } yield extension.walls(orientation.opposite)

    def touchesT = for {
      dir <- Direction.all
      extension <- location.neighbor(dir)
        if extension.allowsWallPlacement
    } yield extension.walls(orientation.opposite)

    touchesI ++ touchesL ++ touchesT
  }
}
