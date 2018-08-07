package minotaur.io

import minotaur.model._

object GameStatePrinter {
  def apply(gs: GameState): String = {
    "\n" +
    "Walls left, black: " + gs.walls(Black) + ", white " + gs.walls(White) +
    "\n" +
    BoardPrinter.printWithCoords(gs.board)
  }

  private def getWalls(gs: GameState, orientation: Orientation): String = {
    val coords = Coordinates(gs.board.boardType)
    gs.board.walls.toList.filter(_.orientation == orientation)
      .sortBy(_.location.location).map(coords.forWall).mkString
  }

  def succinct(gs: GameState): String = {
    "%c:%d:%d,%c:%d:%d,%s:%s".format(
      gs.onTurn.pawn,
      gs.board.pawnLocation(gs.onTurn).location,
      gs.walls(gs.onTurn),
      gs.onTurn.other.pawn,
      gs.board.pawnLocation(gs.onTurn.other).location,
      gs.walls(gs.onTurn.other),
      getWalls(gs, Horizontal),
      getWalls(gs, Vertical)
    )
  }
}
