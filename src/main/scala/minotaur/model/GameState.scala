package minotaur.model

import minotaur.search.AStar

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  private def getPossiblePawnMovements: Set[PawnMovement] =
    board.possibleMoves(onTurn).map(PawnMovement(onTurn, _, this))

  private def getPossibleWallPlacements: Set[WallPlacement] =
    if (walls(onTurn) == 0) Set()
    else board.possibleWalls.map(WallPlacement(onTurn, _, this))
      .filter(wallPlacement => {
        // filter path-blocking walls
        val gs = wallPlacement.apply
        AStar.findPath(
          gs.board, gs.board.pawnLocation(onTurn), onTurn.destination
        ).isDefined
      })

  def getChildren: Set[GameState] =
    (getPossiblePawnMovements ++ getPossibleWallPlacements)
      .map(_.apply)
}
