package minotaur.mcts

import scala.annotation.tailrec
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import minotaur.model.{ GameState, Move, MoveGenerator, Player }
import org.slf4j.LoggerFactory
import profile.Profiler

class MCTS(
  val iterations: Int = 10000,
  val moveGenerator: MoveGenerator = new MoveGenerator(),
  val threads: Int = 4
) {
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

    while (root.visited < iterations) {
      val timeDiff = System.nanoTime() - time
      if (timeDiff > 1000000000) {
        log.info("%3.2f sec; %s".format(timeDiff / 1000000000.0, root))
        time = System.nanoTime()
      }

      node = root

      while (node.isFullyExplored && !node.wins) {
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

      Profiler.profile("MCTS Backprop", backpropagate(expanded, winners))
    }

    log.info("finished at; %s".format(root))

    root.bestChild
  }

  def playout(gameState: GameState): Player = {
    var gs = gameState
    var move: Move = null

    do {
      move = moveGenerator.randomMove(gs)
      gs = move.play
    } while (! move.wins)

    // player of the winning move
    move.gameState.onTurn
  }

  @tailrec private def backpropagate(
    node: Node, winners: List[Player]
  ): Unit = {
    winners.map(node.update)

    if (node.parent.isDefined)
      backpropagate(node.parent.get, winners)
  }
}
