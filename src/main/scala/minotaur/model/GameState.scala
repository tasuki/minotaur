package minotaur.model

import minotaur.search.AStar

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  private def getPossiblePawnMovements: Set[PawnMovement] =
    board.possibleMoves(onTurn).map(PawnMovement(onTurn, _))

  private def getPossibleWallPlacements: Set[WallPlacement] =
    if (walls(onTurn) == 0) Set()
    else board.possibleWalls.map(WallPlacement(onTurn, _))
      .filter(wallPlacement => {
        // filter path-blocking walls
        val gs = wallPlacement.apply(this)
        AStar.findPath(
          gs.board, gs.board.pawnLocation(onTurn), onTurn.destination
        ).isDefined
      })

  def getPossibleMoves: Set[Move] =
    getPossiblePawnMovements ++ getPossibleWallPlacements
}
