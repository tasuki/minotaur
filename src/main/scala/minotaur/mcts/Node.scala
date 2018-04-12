package minotaur.mcts

import java.lang.Math.{ log, sqrt }

import scala.collection.mutable.ListBuffer

import minotaur.model.{ GameState, Move, Player }

class Node(
  val gameState: GameState,
  val move: Option[Move],
  var parent: Option[Node],
  val children: ListBuffer[Node] = ListBuffer[Node](),
  unexplored: Option[Iterator[Node]] = None,
  var winCount: Int = 0,
  var visited: Int = 0
) {
  val wins: Boolean = move.exists(_.wins)
  val unexploredChildren: Iterator[Node] = unexplored.getOrElse(
    gameState.lazyShuffledLegalMoves.map(new Node(_, this))
  )

  var UCT: Double = 0.0

  def this(gameState: GameState) = {
    this(gameState, None, None)
  }

  def this(move: Move, parentNode: Node) = {
    this(move.play, Some(move), Some(parentNode))
  }

  override lazy val hashCode: Int = gameState.hashCode

  override def toString: String =
    f"${gameState.onTurn.other} " +
    f"confidence: ${winRatio()}%1.3f ($winCount / $visited)"

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
    children.foreach(_.updateUCT())
  }

  def updateUCT(): Unit =
    UCT = (winCount.toDouble / visited) +
      move.get.priority *
      1.4 * sqrt(log(parent.get.visited.toDouble) / visited)

  def winRatio(): Double =
    winCount.toDouble / visited

  private def winRatio(node: Node): Double =
    node.winRatio()

  def bestChild: Node =
    children.maxBy(winRatio)

  def bestChildren(count: Int): ListBuffer[Node] =
    children.sortBy(winRatio).reverse.take(count)

  def toRoot: Node = {
    parent = None
    this
  }
}
