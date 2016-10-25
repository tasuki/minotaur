package minotaur.mcts

import scala.annotation.tailrec
import org.slf4j.LoggerFactory
import minotaur.model.{GameState,Player,MoveGenerator,Move}

import profile.Profiler

case class MCTS(iterations: Int = 10000) {
  val log = LoggerFactory.getLogger("MCTS")

  def findMove(
    gameState: GameState
  ): Node = {
    findMove(gameState, new Node(gameState))
  }

  def findMove(
    gameState: GameState,
    root: Node
  ): Node = {
    log.info("carried over; %s".format(root))

    var node: Node = null
    var time = System.nanoTime()

    for (i <- 1 to iterations) {
      val timeDiff = System.nanoTime() - time
      if (timeDiff > 1000000000) {
        log.info("%3.2f sec; %s".format(timeDiff / 1000000000.0, root))
        time = System.nanoTime()
      }

      node = root

      while (node.isFullyExplored && node.wins == false) {
        node = Profiler.profile("MCTS Select child", node.selectChild)
      }

      val expanded: Node =
        if (node.wins) node
        else Profiler.profile("MCTS Expand", node.expand)

      val winner =
        if (expanded.wins) expanded.gameState.onTurn.other
        else Profiler.profile("MCTS Playout", playout(expanded.gameState))

      Profiler.profile("MCTS Backprop", backpropagate(expanded, winner))
    }

    log.info("finished at; %s".format(root))

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
    node: Node, winner: Player
  ): Unit = {
    node.update(winner)

    if (node.parent.isDefined)
      backpropagate(node.parent.get, winner)
  }
}
