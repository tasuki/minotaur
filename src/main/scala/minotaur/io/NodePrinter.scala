package minotaur.io

import minotaur.mcts.Node

object NodePrinter {
  def printTopMoves(parent: Node, topX: Int = 5): List[String] = {
    parent.bestChildren(topX).toList.map { node =>
      node.move.map { move =>
        node.toString + " " + MovePrinter.print(move)
      }.getOrElse("")
    }
  }
}
