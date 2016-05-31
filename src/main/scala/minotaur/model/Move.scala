package minotaur.model

trait Move {
  def apply(gameState: GameState): GameState
}

case class WallPlacement(
  player: Player,
  wall: Wall
) extends Move {
  def apply(gs: GameState) =
    GameState(
      gs.board.copy(walls = gs.board.walls + wall),
      gs.walls + (player -> (gs.walls(player) - 1)),
      player.next
    )
}

case class PawnMovement(
  player: Player,
  location: Location
) extends Move {
  def apply(gs: GameState) =
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
