package minotaur.model

trait Move {
  val apply: GameState
}

case class WallPlacement(
  player: Player,
  wall: Wall,
  gs: GameState
) extends Move {
  val apply =
    GameState(
      gs.board.copy(walls = gs.board.walls + wall),
      gs.walls + (player -> (gs.walls(player) - 1)),
      player.next
    )
}

case class PawnMovement(
  player: Player,
  location: Location,
  gs: GameState
) extends Move {
  val apply =
    GameState(
      gs.board.copy(
        pawns = gs.board.pawns
          - gs.board.pawnLocation(player)
          + (location -> player)
      ),
      gs.walls,
      player.next
    )
}
