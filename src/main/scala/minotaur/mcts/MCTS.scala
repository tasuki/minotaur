package minotaur.mcts

import scala.annotation.tailrec
import minotaur.model.{GameState,Player,MoveGenerator,Move}

object MCTS {
  def findMove(
    gameState: GameState,
    iterations: Int = 10000
  ): MoveNode = {
    val root = new RootNode(gameState)
    var node: Node = null

    for (i <- 1 to iterations) {
      node = root

      // Select
      while (node.isFullyExplored && node.wins == false) {
        node = node.selectChild
      }

      if (node.wins) {
        backpropagate(Some(node), node.gameState.onTurn.other)
      } else {
        // Expand
        val expanded: MoveNode = node.expand

        val winner =
          if (expanded.wins) expanded.move.gameState.onTurn
          // Playout
          else playout(expanded.gameState)

        backpropagate(Some(expanded), winner)
      }
    }

    root.bestChild
  }

  def playout(gameState: GameState): Player = {
    var gs = gameState
    var move: Move = null

    do {
      move = MoveGenerator.randomMove(gs)
      gs = move.play
    } while (! move.wins)

    // player of the winning move
    move.gameState.onTurn
  }

  @tailrec private def backpropagate(
    optNode: Option[Node], winner: Player
  ): Unit = {
    if (optNode.isDefined) {
      val node = optNode.get
      node.update(winner)
      backpropagate(node.parent, winner)
    }
  }
}
