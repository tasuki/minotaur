package minotaur.model

import util.Random
import minotaur.search.AStar

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  val pawnMovementProbability = 50

  def random[T](s: Set[T]) = {
    val n = Random.nextInt(s.size)
    val it = s.iterator.drop(n)
    it.next
  }

  private def getPossiblePawnMovements: Set[PawnMovement] =
    board.possibleMoves(onTurn).map(PawnMovement(_, this))

  private def getPossibleWallPlacements: Set[WallPlacement] =
    if (walls(onTurn) == 0) Set()
    else board.possibleWalls.map(WallPlacement(_, this))
      .filter(isWallPlacementValid)

  private def isWallPlacementValid(wp: WallPlacement): Boolean = {
    val gs = wp.apply
    Player.all.map(player => AStar.findPath(
      gs.board, gs.board.pawnLocation(player), player.destination
    )).filter(_.isEmpty).length == 0
  }

  def getChildren: Set[GameState] =
    (getPossiblePawnMovements ++ getPossibleWallPlacements)
      .map(_.apply)

  private def randomPawnMovement: GameState =
    PawnMovement(random(board.possibleMoves(onTurn)), this).apply

  def getRandomChild: GameState = {
    if (Random.nextInt < pawnMovementProbability || walls(onTurn) == 0)
      randomPawnMovement
    else
      for (wall <- Random.shuffle(board.possibleWalls.toList)) {
        var wp = WallPlacement(wall, this)
        if (isWallPlacementValid(wp))
          return wp.apply
      }

      randomPawnMovement
  }
}
