package minotaur.mcts

import minotaur.io.BoardPrinter
import org.slf4j.LoggerFactory

object Debug {
  val log = LoggerFactory.getLogger("MCTS Debug")

  def printShortInfo(node: Node): Unit = {
    log.debug(node.toString)
    log.debug(BoardPrinter.print(node.gameState.board))
  }

  def printFullInfo(node: Node, topX: Int = 5): Unit = {
    log.debug(s"top $topX moves:")
    node.bestChildren(topX).foreach {
      n => {
        log.debug("")
        log.debug(n.toString)
        log.debug(BoardPrinter.print(n.gameState.board))
      }
    }

    log.debug("")
    log.debug("")
    log.debug("")

    log.debug("sequence of best found moves:")
    var tmpNode = node
    while (true) {
      log.debug("")
      log.debug(tmpNode.toString)
      log.debug(BoardPrinter.print(tmpNode.gameState.board))
      if (tmpNode.children.isEmpty) return
      tmpNode = tmpNode.bestChild
    }
  }
}
