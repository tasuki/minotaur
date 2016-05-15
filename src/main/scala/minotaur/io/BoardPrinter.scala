package minotaur.io

import minotaur.model.{Board,Location}
import minotaur.model.{Direction,South,West}

object BoardPrinter {
  def print(board: Board): String = {
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

    val oddLines = (-1 to boardType.size - 1)
      .map(row => (0 to boardType.size - 1).map(column => {
        val optLoc = getOptionalLocation(row*boardType.size + column)
        if (canGo(optLoc, South)) "+   "
        else "+---"
      }).mkString + "+")

    val evenLines = (0 to boardType.size - 1)
      .map(row => (0 to boardType.size - 1).map(column => {
        val optLoc = getOptionalLocation(row*boardType.size + column)
        val side =
          if (canGo(optLoc, West)) " "
          else "|"

        val pawn = if (optLoc == Some(board.black)) "x"
          else if (optLoc == Some(board.white)) "o"
          else " "

        side + " " + pawn + " "
      }).mkString.replaceAll("""\s+$""",""))

    List(oddLines, evenLines).flatMap(_.zipWithIndex)
      .sortBy(_._2).map(_._1).mkString("\n") + "\n"
  }
}
