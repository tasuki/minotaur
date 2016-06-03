package minotaur.model

import util.Random

object MoveGenerator {
  val pawnMovementProbability = 50

  private def random[T](s: Set[T]) = {
    val n = Random.nextInt(s.size)
    val it = s.iterator.drop(n)
    it.next
  }

  def randomMove(gs: GameState): Move = {
    def randomPawnMove =
      PawnMovement(random(gs.board.possibleMoves(gs.onTurn)), gs)

    if (Random.nextInt(100) < pawnMovementProbability || gs.walls(gs.onTurn) == 0)
      randomPawnMove
    else
      for (wall <- Random.shuffle(gs.board.placeableWalls.toList)) {
        val wp = WallPlacement(wall, gs)
        if (wp.isValid)
          return wp
      }

    randomPawnMove
  }
}
