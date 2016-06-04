package minotaur.model

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

  lazy val play = {
    // blocked movements
    val movementUpdates: Seq[(Location, Seq[Direction])] =
      wall.blocksMovement.map {
        case (location, direction) => (
          location,
          gs.board.allowedMovements(location).filterNot(_ == direction)
        )
      }

    // make paths potential
    val paths = gs.board.shortestPath.map(_ match {
      case (player, Some(path)) => (player -> Some(path.potentialize))
      case (player, None) => (player -> None)
    })

    gs.copy(
      board = gs.board.copy(
        walls = gs.board.walls + wall,
        placeableWalls = gs.board.placeableWalls - wall -- wall.overlaps,
        allowedMovements = gs.board.allowedMovements ++ movementUpdates
      )(cachedPaths = paths),
      walls = gs.walls + (gs.onTurn -> (gs.walls(gs.onTurn) - 1)),
      onTurn = gs.onTurn.next
    )
  }

  // make sure each player can reach their goal
  def isValid: Boolean = play.board.isValid

  val wins = false
}

case class PawnMovement(
  location: Location,
  gameState: GameState
) extends Move {
  private val gs = gameState

  lazy val play = {
    // update paths with current advance if following
    val paths = gs.board.shortestPath + (
      gs.onTurn -> (gs.board.shortestPath(gs.onTurn) match {
        case Some(path: Path) if (path.startsWith(location)) => {
          Option(path.advance)
        }
        case _ => None
      })
    )

    gs.copy(
      board = gs.board.copy(
        pawns = gs.board.pawns
          - gs.board.pawnLocation(gs.onTurn)
          + (location -> gs.onTurn)
      )(cachedPaths = paths),
      onTurn = gs.onTurn.next
    )
  }

  lazy val wins = {
    val player = gs.onTurn

    play.board.pawnLocation(player).isBorder(player.destination)
  }
}
