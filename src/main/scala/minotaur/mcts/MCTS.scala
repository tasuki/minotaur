package minotaur.mcts

import minotaur.model.{GameState,Player,MoveGenerator,Move}

object MCTS {
  def findMove(gameState: GameState): Move = {
    val root = new RootNode(gameState)
    var node: Node = null

    for (i <- 1 to 10000) {
      node = root

      // Select
      while (node.children.isDefined) {
        node = node.selectChild
      }

      // Expand
      node.expand

      // Simulate
      val winner = playout(node.gameState)

      // Backpropagate
      var optNode: Option[Node] = Some(node)
      while (optNode.isDefined) {
        optNode.get.update(winner)
        optNode = optNode.get.parent
      }
    }

    root.children.get.maxBy(_.visited).move
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
