package minotaur.mcts

import Math.{sqrt,log}
import scala.collection.mutable.ListBuffer
import util.Random

import minotaur.model.{GameState,Move,Player}

class Node(
  val gameState: GameState,
  val move: Option[Move],
  var parent: Option[Node],
  val children: ListBuffer[Node],
  unexplored: Option[Iterator[Node]],
  var winCount: Int,
  var visited: Int
) {
  val wins: Boolean = move.map(_.wins).getOrElse(false)
  val unexploredChildren = unexplored.getOrElse(getShuffledChildren)

  var UCT: Double = 0.0

  def this(gameState: GameState) = {
    this(gameState, None, None, ListBuffer[Node](), None, 0, 0)
  }

  def this(move: Move, parentNode: Node) = {
    this(move.play, Some(move), Some(parentNode), ListBuffer[Node](), None, 0, 0)
  }

  def getShuffledChildren: Iterator[Node] =
    Random.shuffle(gameState.getPossibleMoves).toIterator
      .filter(_.isValid)
      .map(new Node(_, this))

  override lazy val hashCode = gameState.hashCode

  override def toString =
    f"${gameState.onTurn.other} " +
    f"confidence: ${winRatio}%1.3f ($winCount / $visited)"

  def isFullyExplored: Boolean =
    unexploredChildren.isEmpty

  def selectChild: Node =
    children.maxBy(_.UCT)

  def expand: Node = {
    val next = unexploredChildren.next
    children += next
    next
  }

  def update(winner: Player): Unit = {
    visited += 1
    if (winner == gameState.onTurn.other) winCount += 1
    children.map(_.updateUCT)
  }

  def updateUCT =
    UCT = (winCount.toDouble / visited) +
      move.get.priority *
      1.4 * sqrt(log(parent.get.visited.toDouble) / visited)

  def winRatio: Double =
    winCount.toDouble / visited

  private def winRatio(node: Node): Double =
    node.winRatio

  def bestChild: Node =
    children.maxBy(winRatio)

  def bestChildren(count: Int): ListBuffer[Node] =
    children.sortBy(winRatio).reverse.take(count)

  def toRoot: Node = {
    parent = None
    this
  }
}
