package minotaur.mcts

import minotaur.model.GameState
import minotaur.io.BoardPrinter

object Debug {
  def printShortInfo(node: MoveNode): Unit = {
    println(node)
    println(BoardPrinter.print(node.move.play.board))
  }

  def printFullInfo(chosen: MoveNode, topX: Int = 5): Unit = {
    println(s"top $topX moves:")
    chosen.parent.get.bestChildren(topX).foreach {
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
    var node: Node = chosen.parent.get
    var state: GameState = null
    while (true) {
      state = node.gameState
      println
      println(node)
      println(BoardPrinter.print(state.board))
      if (node.children.isEmpty) {
        return
      }
      node = node.bestChild
    }
  }
}
