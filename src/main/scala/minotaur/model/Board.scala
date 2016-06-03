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
    )
  }
}

case class Board(
  boardType: BoardType,
  pawns: Map[Location, Player],
  walls: Set[Wall],
  placeableWalls: Set[Wall],
  allowedMovements: Map[Location, Seq[Direction]]
) {
  val size = boardType.size

  def canMove(location: Location, direction: Direction): Boolean =
    allowedMovements(location).contains(direction)

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
}
