package minotaur.model

case class Game(state: GameState, parent: Option[Game])

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  lazy val getPossibleMoves: Seq[Move] = {
    def getPossiblePawnMovements: Seq[PawnMovement] =
      board.possibleMoves(onTurn).map(PawnMovement(_, this))

    def getPossibleWallPlacements: Seq[WallPlacement] =
      if (walls(onTurn) == 0) Seq()
      else board.placeableWalls.map(WallPlacement(_, this))(collection.breakOut)

    (getPossiblePawnMovements ++ getPossibleWallPlacements)
  }
}
