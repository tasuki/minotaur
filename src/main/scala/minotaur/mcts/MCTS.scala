package minotaur.mcts

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import org.slf4j.LoggerFactory
import minotaur.model.{GameState,Player,MoveGenerator,Move}

import profile.Profiler

object MCTS {
  val threads = 4
  val log = LoggerFactory.getLogger("MCTS")

  def findMove(
    gameState: GameState,
    iterations: Int = 10000
  ): MoveNode = {
    val root = new RootNode(gameState)
    var node: Node = null
    var time = System.nanoTime()

    for (i <- 1 to iterations / threads) {
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

      val winners: List[Player] =
        if (expanded.wins) List(expanded.gameState.onTurn.other)
        else Profiler.profile("MCTS Playout",
          Await.result(Future.sequence(List.fill(threads)(
            Future { playout(expanded.gameState) }
          )), Duration.Inf)
        )

      Profiler.profile("MCTS Backprop", backpropagate(Some(expanded), winners))
    }

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
    optNode: Option[Node], winners: List[Player]
  ): Unit = {
    if (optNode.isDefined) {
      val node = optNode.get
      winners.map(node.update)
      backpropagate(node.parent, winners)
    }
  }
}
