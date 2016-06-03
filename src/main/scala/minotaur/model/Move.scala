package minotaur.model

import minotaur.search.AStar

sealed trait Move {
  val gameState: GameState
  val play: GameState
  val wins: Boolean
}

case class WallPlacement(
  wall: Wall,
  gameState: GameState
) extends Move {
  private val gs = gameState

  // use wall, add it to board, block movement, switch whose turn it is
  lazy val play = {
    val movementUpdates: Seq[(Location, Seq[Direction])] =
      wall.blocksMovement.map {
        case (location, direction) => (
          location,
          gs.board.allowedMovements(location).filterNot(_ == direction)
        )
      }

    gs.copy(
      board = gs.board.copy(
        walls = gs.board.walls + wall,
        placeableWalls = gs.board.placeableWalls - wall -- wall.overlaps,
        allowedMovements = gs.board.allowedMovements ++ movementUpdates
      ),
      walls = gs.walls + (gs.onTurn -> (gs.walls(gs.onTurn) - 1)),
      onTurn = gs.onTurn.next
    )
  }

  // make sure each player can reach their goal
  def isValid: Boolean = {
    val gs = this.play
    Player.all.map(player => AStar.findPath(
      gs.board, gs.board.pawnLocation(player), player.destination
    )).filter(_.isEmpty).length == 0
  }

  val wins = false
}

case class PawnMovement(
  location: Location,
  gameState: GameState
) extends Move {
  private val gs = gameState

  // change the pawn position and whose turn it is
  lazy val play =
    gs.copy(
      board = gs.board.copy(
        pawns = gs.board.pawns
          - gs.board.pawnLocation(gs.onTurn)
          + (location -> gs.onTurn)
      ),
      onTurn = gs.onTurn.next
    )

  lazy val wins = {
    val player = gs.onTurn

    play.board.pawnLocation(player).isBorder(player.destination)
  }
}
