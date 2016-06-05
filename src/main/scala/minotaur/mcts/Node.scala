package minotaur.mcts

import Math.{sqrt,log}
import minotaur.model.{GameState,Move,Player}

trait Node {
  val gameState: GameState
  val parent: Option[Node]

  override lazy val hashCode = gameState.hashCode

  var wins = 0
  var visited = 0
  var children: Option[Seq[MoveNode]] = None

  def selectChild: MoveNode = {
    children.get.maxBy(_.UCT)
  }

  def expand: Unit = {
    children = Some(
      gameState.getPossibleMoves.map(new MoveNode(_, this))
    )
  }

  def update(winner: Player): Unit = {
    visited += 1
    if (winner == gameState.onTurn) wins += 1
  }
}

class RootNode(gs: GameState) extends Node {
  val gameState = gs
  val parent = None
}

class MoveNode(val move: Move, parentNode: Node) extends Node {
  val gameState = move.play
  val parent = Some(parentNode)

  override def toString = {
    f"W/V:$wins%4d /$visited%4d = ${wins.toDouble/visited}%1.5f;" +
      f" UCT: ${UCT}%1.5f"
  }

  def UCT: Double = {
    if (visited == 0)
      // prefer non-visited nodes
      return 10.0

    (wins.toDouble / visited) +
      sqrt(log(parentNode.visited.toDouble) / visited)
  }
}
