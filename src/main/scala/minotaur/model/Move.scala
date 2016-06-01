package minotaur.model

trait Move {
  val apply: GameState
}

case class WallPlacement(
  wall: Wall,
  gs: GameState
) extends Move {
  val apply =
    GameState(
      gs.board.copy(walls = gs.board.walls + wall),
      gs.walls + (gs.onTurn -> (gs.walls(gs.onTurn) - 1)),
      gs.onTurn.next
    )
}

case class PawnMovement(
  location: Location,
  gs: GameState
) extends Move {
  val apply =
    GameState(
      gs.board.copy(
        pawns = gs.board.pawns
          - gs.board.pawnLocation(gs.onTurn)
          + (location -> gs.onTurn)
      ),
      gs.walls,
      gs.onTurn.next
    )
}
