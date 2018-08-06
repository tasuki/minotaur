package minotaur.io

import scala.annotation.tailrec

import minotaur.model._

object QuoridorStrats {
  val stratsColumns: List[Char] = "abcdefghi".toList

  val file = "src/test/resources/empty.txt"
  val game = Game(GameState(
    BoardReader.fromFile(file), Map(
      Black -> 10,
      White -> 10
    ), Black
  ), None, None)
  val bt: BoardType = game.state.board.boardType

  def importGame(str: String): Game =
    getGame(str.split(";"), game)

  @tailrec
  private def getGame(moves: Seq[String], parent: Game): Game =
    if (moves.isEmpty) parent
    else {
      val move: Move = getMove(parent.state, moves.head)
      val game = Game(move.play, Some(move), Some(parent))

      //println(GameStatePrinter(game.state))
      getGame(moves.tail, game)
    }

  private def getMove(gs: GameState, move: String): Move =
    move match {
      case wall if move.length == 3 => WallPlacement(getWall(wall), gs)
      case pawn if move.length == 2 => PawnMovement(getLocation(pawn), gs)
    }

  private def getWall(str: String): Wall = {
    val columnIndex = stratsColumns.indexOf(str.charAt(0))
    // flip it, deduce one cause we use upper left and they use lower left
    val rowIndex = bt.size - str.charAt(1).asDigit - 1
    val orientation: Orientation = str.charAt(2) match {
      case 'h' => Horizontal
      case 'v' => Vertical
    }

    val location = columnIndex + bt.size * rowIndex
    Wall(Location(location, bt), orientation)
  }

  private def getLocation(str: String): Location = {
    val columnIndex = stratsColumns.indexOf(str.charAt(0))
    val rowIndex = bt.size - str.charAt(1).asDigit // flip it!

    val location = columnIndex + bt.size * rowIndex
    Location(location, bt)
  }

  def exportGame(game: Game): String =
    game.getMoves().map(toQuoridorStrats).mkString(";")

  private def toQuoridorStrats(move: Move): String =
    move match {
      case wp: WallPlacement => toQsWall(wp)
      case pm: PawnMovement => toQsPawn(pm)
    }

  private def qsLocation(location: Location): (Char, Int) = {
    val column = location.estimateDistance(West)
    val row = location.estimateDistance(South)

    (stratsColumns(column), row)
  }

  private def toQsWall(wp: WallPlacement): String = {
    val (column, row) = qsLocation(wp.wall.location)

    val orientation = wp.wall.orientation match {
      case Horizontal => "h"
      case Vertical => "v"
    }

    column.toString + row.toString + orientation
  }

  private def toQsPawn(pm: PawnMovement): String = {
    val (column, row) = qsLocation(pm.location)

    column.toString + (row + 1).toString
  }
}
