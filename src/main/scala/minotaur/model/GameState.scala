package minotaur.model

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  def getPossibleMoves: Set[PawnMovement] =
    board.possibleMoves(onTurn).map(PawnMovement(onTurn, _))

  def getPossibleWallPlacements: Set[WallPlacement] =
    board.possibleWalls.map(WallPlacement(onTurn, _))
}
