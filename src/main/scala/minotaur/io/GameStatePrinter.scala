package minotaur.io

import minotaur.model._
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

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
      .toList.sortBy(_.location.location).map(coords.forWall).mkString
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

  // unused but unintuitive and might come in handy later
  private def padWalls(walls: INDArray): INDArray =
    Nd4j.pad(walls, Array(Array(0, 1), Array(0, 1)), Nd4j.PadMode.CONSTANT)

  private def getWallMatrix(gs: GameState, orientation: Orientation): INDArray = {
    val ret = Nd4j.zeros(9 * 9)
    val walls = gs.board.walls.toList.filter(_.orientation == orientation)
    walls.foreach(wall => ret.putScalar(wall.location.location, 1))
    ret.reshape(9, 9)
  }

  private def getPawnMatrix(gs: GameState, player: Player): INDArray = {
    val ret = Nd4j.zeros(9 * 9)
    ret.putScalar(gs.board.pawnLocation(player).location, 1)
    ret.reshape(9, 9)
  }

  private def padWallCounts(count: Int): INDArray =
    Nd4j.valueArrayOf(Array(9, 9), count)

  def toNdArray(gs: GameState): INDArray = {
    val data = Nd4j.vstack(
      getWallMatrix(gs, Horizontal),
      getWallMatrix(gs, Vertical),
      getPawnMatrix(gs, gs.onTurn),
      padWallCounts(gs.walls(gs.onTurn)),
      getPawnMatrix(gs, gs.onTurn.other),
      padWallCounts(gs.walls(gs.onTurn.other))
    )

    data.reshape(6, 9, 9)
  }
}
