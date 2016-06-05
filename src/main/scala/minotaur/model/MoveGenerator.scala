package minotaur.model

import util.Random

object MoveGenerator {
  val pawnMovementProbability = 50
  val seekShortestRouteProbability = 50

  private def percentChance(chance: Int): Boolean =
    Random.nextInt(100) < chance

  def randomMove(gs: GameState): Move = {
    def randomPawnMove: PawnMovement = {
      val possibleMoves = gs.board.possibleMoves(gs.onTurn)

      if (percentChance(seekShortestRouteProbability)) {
        gs.board.shortestPath(gs.onTurn).get.path
          .find(possibleMoves contains _)
          .foreach(loc => return PawnMovement(loc, gs))
      }

      PawnMovement(possibleMoves(Random.nextInt(possibleMoves.length)), gs)
    }

    if (percentChance(pawnMovementProbability) || gs.walls(gs.onTurn) < 1)
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
