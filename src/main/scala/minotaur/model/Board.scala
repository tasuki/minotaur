package minotaur.model

object Board {
  def apply(
    boardType: BoardType,
    pawns: Map[Location, Player],
    walls: Set[Wall]
  ): Board = {
    def getAllowedMovements(
      boardType: BoardType,
      walls: Set[Wall]
    ): Map[Location, Seq[Direction]] = {
      val blocked = walls.toList.flatMap(_.blocksMovement)

      boardType.locations.map(location => location -> {
        Direction.all
          .filterNot(location.isBorder(_))
          .filterNot(dir => blocked.contains((location, dir)))
      }).toMap
    }

    Board(
      boardType,
      pawns,
      walls,
      boardType.possibleWalls -- walls -- walls.map(_.overlaps).flatten,
      getAllowedMovements(boardType, walls)
    )(Player.all.map(player => player -> None).toMap)
  }
}

case class Board(
  boardType: BoardType,
  pawns: Map[Location, Player],
  walls: Set[Wall],
  placeableWalls: Set[Wall],
  allowedMovements: Map[Location, Seq[Direction]]
)(cachedPaths: Map[Player, Option[Path]]) {
  val size = boardType.size

  def canMove(location: Location, direction: Direction): Boolean =
    allowedMovements(location).contains(direction)

  def canMove(loc1: Location, loc2: Location): Boolean =
    neighbors(loc1) contains loc2

  def neighbors(location: Location): Seq[Location] =
    allowedMovements(location).map(location.neighbor(_).get)

  def pawnLocation(player: Player): Location =
    pawns.find(_._2 == player).map(_._1).get

  def possibleMoves(player: Player): Set[Location] = {
    val location = pawnLocation(player)

    allowedMovements(location)
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

  def findPath(player: Player) =
    Search.findPath(this, pawnLocation(player), player.destination)

  lazy val shortestPath: Map[Player, Option[Path]] =
    Player.all.map(player => player -> (cachedPaths(player) match {
      case Some(_: ShortestPath) => cachedPaths(player)
      case Some(path: PotentialShortestPath) =>
        if (path.isValid(player, this)) Some(path.validate)
        else findPath(player)
      case None => findPath(player)
    })).toMap

  lazy val isValid: Boolean =
    Player.all.map(shortestPath(_)).filter(_.isEmpty).length == 0
}
