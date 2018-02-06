package minotaur.io

import scala.annotation.tailrec
import minotaur.model._

object QuoridorStrats {
  val stratsColumns = "abcdefghi".toList

  val file = "src/test/resources/empty.txt"
  val game = Game(GameState(
    BoardReader.fromFile(file), Map(
      Black -> 10,
      White -> 10
    ), Black
  ), None)
  val bt = game.state.board.boardType

  def fromString(str: String): Game =
    getGame(str.split(";"), game)

  @tailrec
  private def getGame(moves: Seq[String], parent: Game): Game =
    if (moves.isEmpty) parent
    else {
      val move: Move = getMove(parent.state, moves.head)
      val game = Game(move.play, Some(parent))

      //println(GameStatePrinter(game.state))
      getGame(moves.tail, game)
    }

  private def getMove(gs: GameState, move: String): Move =
    move match {
      case wall if move.length == 3 => getWallPlacement(gs, wall)
      case pawn if move.length == 2 => getPawnMovement(gs, pawn)
    }

  private def getWallPlacement(gs: GameState, str: String): WallPlacement = {
    val columnIndex = stratsColumns.indexOf(str.charAt(0))
    // flip it, deduce one cause we use upper left and they use lower left
    val rowIndex = bt.size - str.charAt(1).asDigit - 1
    val orientation: Orientation = str.charAt(2) match {
      case 'h' => Horizontal
      case 'v' => Vertical
    }

    val location = columnIndex + bt.size * rowIndex
    WallPlacement(Wall(Location(location, bt), orientation), gs)
  }

  private def getPawnMovement(gs: GameState, str: String): PawnMovement = {
    val columnIndex = stratsColumns.indexOf(str.charAt(0))
    val rowIndex = bt.size - str.charAt(1).asDigit // flip it!

    val location = columnIndex + bt.size * rowIndex
    PawnMovement(Location(location, bt), gs)
  }
}
