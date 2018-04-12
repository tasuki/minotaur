package minotaur.model

import scala.util.Random

case class Game(state: GameState, parent: Option[Game])

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  private lazy val possibleMoves: Seq[Move] = {
    def getPossiblePawnMovements: Seq[PawnMovement] =
      board.possibleMoves(onTurn).map(PawnMovement(_, this))

    def getPossibleWallPlacements: Seq[WallPlacement] =
      if (walls(onTurn) == 0) Nil
      else board.placeableWalls.map(WallPlacement(_, this))(collection.breakOut)

    getPossiblePawnMovements ++ getPossibleWallPlacements
  }

  def legalMoves: Seq[Move] =
    possibleMoves.filter(_.isValid)

  def lazyShuffledLegalMoves: Iterator[Move] =
    Random.shuffle(possibleMoves).toIterator.filter(_.isValid)
}
