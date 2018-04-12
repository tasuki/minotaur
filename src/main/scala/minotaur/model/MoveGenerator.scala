package minotaur.model

import scala.util.Random

import profile.Profiler

class MoveGenerator(
  val pawnMovementProbability: Int = 90,
  val seekShortestRouteProbability: Int = 90
) {
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

    def randomWallMove: Option[WallPlacement] = {
      for (wall <- Random.shuffle(gs.board.placeableWalls.toList)) {
        val wp = WallPlacement(wall, gs)
        if (wp.isValid) return Some(wp)
      }

      None
    }

    if (percentChance(pawnMovementProbability) || gs.walls(gs.onTurn) < 1)
      Profiler.profile("Random pawn move", randomPawnMove)
    else
      Profiler.profile("Random wall move", randomWallMove.getOrElse(randomPawnMove))
  }
}
