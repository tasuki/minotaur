package minotaur.model

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  lazy val getChildren: Set[GameState] = {
    def getPossiblePawnMovements: Set[PawnMovement] =
      board.possibleMoves(onTurn).map(PawnMovement(_, this))

    def getPossibleWallPlacements: Set[WallPlacement] =
      if (walls(onTurn) == 0) Set()
      else board.placeableWalls.map(WallPlacement(_, this))
        .filter(_.isValid)

    (getPossiblePawnMovements ++ getPossibleWallPlacements)
      .map(_.play)
  }
}
