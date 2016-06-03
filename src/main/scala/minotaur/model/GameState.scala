package minotaur.model

import util.Random

case class GameState(
  board: Board,
  walls: Map[Player, Int],
  onTurn: Player
) {
  val pawnMovementProbability = 50

  private def random[T](s: Set[T]) = {
    val n = Random.nextInt(s.size)
    val it = s.iterator.drop(n)
    it.next
  }

  lazy val getChildren: Set[GameState] = {
    def getPossiblePawnMovements: Set[PawnMovement] =
      board.possibleMoves(onTurn).map(PawnMovement(_, this))

    def getPossibleWallPlacements: Set[WallPlacement] =
      if (walls(onTurn) == 0) Set()
      else board.placeableWalls.map(WallPlacement(_, this))
        .filter(_.isValid)

    (getPossiblePawnMovements ++ getPossibleWallPlacements)
      .map(_.play)
  }

  def randomMove: Move = {
    def randomPawnMove =
      PawnMovement(random(board.possibleMoves(onTurn)), this)

    if (Random.nextInt < pawnMovementProbability || walls(onTurn) == 0)
      randomPawnMove
    else
      for (wall <- Random.shuffle(board.placeableWalls.toList)) {
        val wp = WallPlacement(wall, this)
        if (wp.isValid)
          return wp
      }

    randomPawnMove
  }
}
