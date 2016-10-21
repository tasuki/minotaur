package minotaur.mcts

import org.slf4j.LoggerFactory
import minotaur.model.GameState
import minotaur.io.BoardPrinter

object Debug {
  val log = LoggerFactory.getLogger("MCTS Debug")

  def printShortInfo(node: MoveNode): Unit = {
    log.debug(node.toString)
    log.debug(BoardPrinter.print(node.move.play.board))
  }

  def printFullInfo(chosen: MoveNode, topX: Int = 5): Unit = {
    log.debug(s"top $topX moves:")
    chosen.parent.get.bestChildren(topX).foreach {
      mn => {
        log.debug("")
        log.debug(mn.toString)
        log.debug(BoardPrinter.print(mn.move.play.board))
      }
    }

    log.debug("")
    log.debug("")
    log.debug("")

    log.debug("sequence of best found moves:")
    var node: Node = chosen.parent.get
    var state: GameState = null
    while (true) {
      state = node.gameState
      log.debug("")
      log.debug(node.toString)
      log.debug(BoardPrinter.print(state.board))
      if (node.children.isEmpty) {
        return
      }
      node = node.bestChild
    }
  }
}
