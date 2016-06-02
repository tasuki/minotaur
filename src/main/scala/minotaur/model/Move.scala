package minotaur.model

import minotaur.search.AStar

trait Move {
  val play: GameState
}

case class WallPlacement(
  wall: Wall,
  gs: GameState
) extends Move {
  val play =
    gs.copy(
      board = gs.board.copy(walls = gs.board.walls + wall),
      walls = gs.walls + (gs.onTurn -> (gs.walls(gs.onTurn) - 1)),
      onTurn = gs.onTurn.next
    )

  def isValid: Boolean = {
    val gs = this.play
    Player.all.map(player => AStar.findPath(
      gs.board, gs.board.pawnLocation(player), player.destination
    )).filter(_.isEmpty).length == 0
  }
}

case class PawnMovement(
  location: Location,
  gs: GameState
) extends Move {
  val play =
    gs.copy(
      board = gs.board.copy(
        pawns = gs.board.pawns
          - gs.board.pawnLocation(gs.onTurn)
          + (location -> gs.onTurn)
      ),
      onTurn = gs.onTurn.next
    )
}
