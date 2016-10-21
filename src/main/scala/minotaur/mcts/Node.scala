package minotaur.mcts

import Math.{sqrt,log}
import scala.collection.mutable.ListBuffer
import util.Random

import minotaur.model.{GameState,Move,Player}

trait Node {
  val gameState: GameState
  val parent: Option[Node]
  val wins: Boolean

  var winCount = 0
  var visited = 0

  val children: ListBuffer[MoveNode] = ListBuffer[MoveNode]()

  private lazy val unexploredChildren: Iterator[MoveNode] =
    Random.shuffle(gameState.getPossibleMoves).toIterator
      .filter(_.isValid)
      .map(new MoveNode(_, this))

  override lazy val hashCode = gameState.hashCode

  override def toString =
    f"${gameState.onTurn.other} " +
    f"confidence: ${winRatio}%1.3f ($winCount / $visited)"

  def isFullyExplored: Boolean =
    unexploredChildren.isEmpty

  def selectChild: MoveNode =
    children.maxBy(_.UCT)

  def expand: MoveNode = {
    val next = unexploredChildren.next
    children += next
    next
  }

  def update(winner: Player): Unit = {
    visited += 1
    if (winner == gameState.onTurn.other) winCount += 1
    children.map(_.updateUCT)
  }

  def winRatio: Double =
    winCount.toDouble / visited

  private def winRatio(node: Node): Double =
    node.winRatio

  def bestChild: MoveNode =
    children.maxBy(winRatio)

  def bestChildren(count: Int): ListBuffer[MoveNode] =
    children.sortBy(winRatio).reverse.take(count)
}

class RootNode(gs: GameState) extends Node {
  val gameState = gs
  val parent = None
  val wins = false
}

class MoveNode(val move: Move, parentNode: Node) extends Node {
  val gameState = move.play
  val parent = Some(parentNode)
  val wins = move.wins
  var UCT: Double = 0.0

  def updateUCT = {
    UCT = profile.Profiler.profile("Node UCT",
      (winCount.toDouble / visited) + move.priority *
      1.4 * sqrt(log(parentNode.visited.toDouble) / visited)
    )
  }
}
