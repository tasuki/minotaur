package minotaur.model

case class Board(
  boardType: BoardType,
  pawns: Map[Location, Player],
  walls: Set[Wall],
  possibleWalls: Set[Wall]
) {
  def this(
    boardType: BoardType,
    pawns: Map[Location, Player],
    walls: Set[Wall]
  ) = this(
    boardType,
    pawns,
    walls,
    boardType.possibleWalls -- walls -- walls.map(_.overlaps).flatten
  )

  val size = boardType.size

  def canMove(location: Location, direction: Direction): Boolean = {
    if (location.isBorder(direction))
      return false

    val locationsToCheck: Seq[Option[Location]] = direction match {
      case North => Seq(
        location.neighbor(North),
        location.neighbor(North).flatMap(_.neighbor(West)))
      case South => Seq(Some(location), location.neighbor(West))
      case East => Seq(Some(location), location.neighbor(North))
      case West => Seq(
        location.neighbor(West),
        location.neighbor(West).flatMap(_.neighbor(North)))
    }

    locationsToCheck.flatten
      .filter(_.allowsWallPlacement)
      .filter(walls contains Wall(_, direction.orientation.opposite))
      .length == 0
  }

  def neighbors(location: Location): Seq[Location] =
    Direction.all.filter(canMove(location, _))
      .map(location.neighbor(_).get)

  def pawnLocation(player: Player): Location =
    pawns.find(_._2 == player).map(_._1).get

  def possibleMoves(player: Player): Set[Location] = {
    val location = pawnLocation(player)
    Direction.all.filter(canMove(location, _))
      .map(dir => (dir, location.neighbor(dir).get))
      .map(_ match {
        case (dir, neighbor) if pawns.isDefinedAt(neighbor) =>
          if (canMove(neighbor, dir))
            // jump straight over piece
            Seq(neighbor.neighbor(dir).get)
          else
            // jump sideways
            dir.orientation.opposite.directions
              .filter(canMove(neighbor, _))
              .map(neighbor.neighbor(_).get)
        case (_, neighbor) => Seq(neighbor)
      }).flatten.toSet
  }
}
