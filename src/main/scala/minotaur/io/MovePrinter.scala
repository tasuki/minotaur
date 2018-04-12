package minotaur.io

import minotaur.model.{ Move, PawnMovement, WallPlacement }

object MovePrinter {
  def print(move: Move): String = {
    val coords = Coordinates(move.gameState.board.boardType)
    move match {
      case w: WallPlacement => coords.forWall(w.wall)
      case p: PawnMovement =>
        val from = p.gameState.board.pawnLocation(p.gameState.onTurn)
        val to = p.location

        from.neighbor.toSeq
          .filter { case (_, optLoc) => optLoc.contains(to) }
          .map { case (dir, _) => dir }
          .headOption.map(_.toString).getOrElse("jump")
    }
  }
}
