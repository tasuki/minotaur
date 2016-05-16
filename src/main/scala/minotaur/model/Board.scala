package minotaur.model

case class Board(
  boardType: BoardType,
  black: Location,
  white: Location,
  walls: Set[Wall] = Set()
) {
  val size = boardType.size

  def canMove(location: Location, direction: Direction): Boolean = {
    if (! location.neighbor(direction).isDefined)
      return false

    val locationsToCheck: Seq[Option[Location]] = direction match {
      case North => Seq(
        location.neighbor(North),
        location.neighbor(North).flatMap(l => l.neighbor(West)))
      case South => Seq(Some(location), location.neighbor(West))
      case East => Seq(Some(location), location.neighbor(North))
      case West => Seq(
        location.neighbor(West),
        location.neighbor(West).flatMap(l => l.neighbor(North)))
    }

    locationsToCheck.flatten
      .filter(l => l.allowsWallPlacement)
      .filter(l => walls contains Wall(l, direction.orientation.opposite))
      .length == 0
  }

  def neighbors(location: Location): Seq[Location] = {
    Direction.all.filter(dir => canMove(location, dir))
      .map(dir => location.neighbor(dir).get)
  }
}
