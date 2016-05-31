package minotaur.model

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
)
