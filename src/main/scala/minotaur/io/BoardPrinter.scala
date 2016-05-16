package minotaur.io

import minotaur.model.{Board,Location}
import minotaur.model.{Direction,South,West}

object BoardPrinter {
  private def printWithCellContent(board: Board, cellContent: Option[Location] => String) = {
    val boardType = board.boardType

    def getOptionalLocation(position: Int): Option[Location] = {
      Option(position)
        .filter(pos => boardType.containsLocation(pos))
        .map(pos => Location(pos, boardType))
    }

    def canGo(location: Option[Location], direction: Direction): Boolean = {
      location
        .filter(l => !l.isBorder(direction) && !board.canMove(l, direction))
        .map(l => false).getOrElse(true)
    }

    val oddLines = (-1 to board.size - 1)
      .map(row => (0 to board.size - 1).map(column => {
        val optLoc = getOptionalLocation(row*board.size + column)
        if (canGo(optLoc, South)) "+   "
        else "+---"
      }).mkString + "+")

    val evenLines = (0 to board.size - 1)
      .map(row => (0 to board.size - 1).map(column => {
        val optLoc = getOptionalLocation(row*board.size + column)
        val side =
          if (canGo(optLoc, West)) " "
          else "|"

        side + cellContent(optLoc)
      }).mkString.replaceAll("""\s+$""",""))

    List(oddLines, evenLines).flatMap(_.zipWithIndex)
      .sortBy(_._2).map(_._1).mkString("\n") + "\n"
  }

  def print(board: Board): String = {
    printWithCellContent(board, (optLoc: Option[Location]) => {
      val pawn = if (optLoc == Some(board.black)) "x"
        else if (optLoc == Some(board.white)) "o"
        else " "

      " " + pawn + " "
    })
  }
}
