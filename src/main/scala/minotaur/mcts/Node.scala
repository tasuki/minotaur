package minotaur.mcts

import Math.{sqrt,log}
import scala.collection.mutable.ListBuffer
import util.Random

import minotaur.model.{GameState,Move,Player}

trait Node {
  val gameState: GameState
  val parent: Option[Node]

  override def toString = {
    f"${gameState.onTurn.other} confidence: ${wins.toDouble/visited}%1.2f ($wins / $visited)"
  }

  override lazy val hashCode = gameState.hashCode

  var wins = 0
  var visited = 0

  private lazy val unexploredChildren: Iterator[MoveNode] =
    Random.shuffle(gameState.getPossibleMoves).toIterator
      .filter(_.isValid)
      .map(new MoveNode(_, this))

  def isFullyExplored: Boolean =
    unexploredChildren.isEmpty

  val children: ListBuffer[MoveNode] = ListBuffer[MoveNode]()

  def selectChild: MoveNode =
    children.maxBy(_.UCT)

  def expand: MoveNode = {
    val next = unexploredChildren.next
    children += next
    next
  }

  def update(winner: Player): Unit = {
    visited += 1
    if (winner == gameState.onTurn.other) wins += 1
  }
}

class RootNode(gs: GameState) extends Node {
  val gameState = gs
  val parent = None
}

class MoveNode(val move: Move, parentNode: Node) extends Node {
  val gameState = move.play
  val parent = Some(parentNode)

  def UCT: Double = {
    (wins.toDouble / visited) +
      sqrt(log(parentNode.visited.toDouble) / visited)
  }
}
