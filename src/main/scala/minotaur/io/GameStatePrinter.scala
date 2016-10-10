package minotaur.io

import minotaur.model.{GameState,Black,White}
import minotaur.model.{Wall,Orientation,Horizontal,Vertical}

object GameStatePrinter {
  def apply(gs: GameState): Unit = {
    println
    println("Walls left, black: " + gs.walls(Black) + ", white " + gs.walls(White))
    println
    print(BoardPrinter.printWithCoords(gs.board))
  }

  def getWalls(gs: GameState, orientation: Orientation): String = {
    val coords = Coordinates(gs.board.boardType)
    gs.board.walls.toList.filter(_.orientation == orientation)
      .toList.sortBy(_.location.location).map(coords.forWall(_)).mkString
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
