package minotaur.model

import util.Random

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
      .filter(_.isValid)

  def getChildren: Set[GameState] =
    (getPossiblePawnMovements ++ getPossibleWallPlacements)
      .map(_.play)

  private def randomPawnMovement: GameState =
    PawnMovement(random(board.possibleMoves(onTurn)), this).play

  def getRandomChild: GameState = {
    if (Random.nextInt < pawnMovementProbability || walls(onTurn) == 0)
      randomPawnMovement
    else
      for (wall <- Random.shuffle(board.possibleWalls.toList)) {
        val wp = WallPlacement(wall, this)
        if (wp.isValid)
          return wp.play
      }

      randomPawnMovement
  }
}
