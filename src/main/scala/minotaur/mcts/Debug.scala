package minotaur.mcts

import minotaur.model.GameState
import minotaur.io.BoardPrinter

object Debug {
  def printShortInfo(node: MoveNode): Unit = {
    println(node)
    println(BoardPrinter.print(node.move.play.board))
  }

  def printFullInfo(node: MoveNode): Unit = {
    println("top 5 moves:")
    node.parent.get.children.sortBy(_.visited).reverse.take(5).foreach {
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
    var mn: Node = node.parent.get
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
