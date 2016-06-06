package minotaur.mcts

import minotaur.model.{GameState,Player,MoveGenerator,Move}
import minotaur.io.BoardPrinter

object MCTS {
  def findMove(gameState: GameState): Move = {
    val root = new RootNode(gameState)
    var node: Node = null

    for (i <- 1 to 10000) {
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

    root.children.maxBy(_.visited).move
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

  def debug(node: Node): Unit = {
    println("top 5 moves:")
    node.children.sortBy(_.visited).reverse.take(5).foreach {
      mn => {
        println
        println(mn)
        println(BoardPrinter.print(mn.move.play.board))
      }
    }

    println
    println
    println

    println("sequence of best found moves:")
    var mn: Node = node
    var state: GameState = null
    do {
      state = mn.gameState
      println
      println(mn)
      println(BoardPrinter.print(state.board))
      mn = mn.children.maxBy(_.visited)
    } while (mn.children.nonEmpty)
  }
}
