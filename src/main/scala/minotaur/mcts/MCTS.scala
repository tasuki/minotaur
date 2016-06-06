package minotaur.mcts

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
      while (node.isFullyExplored) {
        node = node.selectChild
      }

      // Expand
      val expanded: Node = node.expand

      // Simulate
      val winner = playout(expanded.gameState)

      // Backpropagate
      var optNode: Option[Node] = Some(expanded)
      while (optNode.isDefined) {
        optNode.get.update(winner)
        optNode = optNode.get.parent
      }
    }

    root.children.maxBy(_.visited)
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
}
