package minotaur.mcts

import minotaur.model.GameState
import minotaur.io.BoardPrinter

object Debug {
  def printShortInfo(node: MoveNode): Unit = {
    println(node)
    println(BoardPrinter.print(node.move.play.board))
  }

  def printFullInfo(chosen: MoveNode): Unit = {
    println("top 5 moves:")
    chosen.parent.get.children.sortBy(_.visited).reverse.take(5).foreach {
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
      node = node.children.maxBy(_.visited)
    }
  }
}
