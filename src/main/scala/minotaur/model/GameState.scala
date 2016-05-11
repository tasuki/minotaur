package minotaur.model

case class GameState(
  board: Board,
  blackWalls: Int,
  whiteWalls: Int,
  whiteOnTurn: Boolean
)
