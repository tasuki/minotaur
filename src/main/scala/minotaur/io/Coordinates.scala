package minotaur.io

import minotaur.model.{BoardType,Wall}
import minotaur.model.{Horizontal,Vertical}
import minotaur.model.{North,West}

case class Coordinates(boardType: BoardType) {
  val vertical   = getCoords("abcdfghijklmopr")
  val horizontal = getCoords("123456789tuvxyz")

  private def getCoords(possibilities: String): List[Char] =
    possibilities.toList.take(boardType.size - 1)

  def exist(verticalCoord: Char, horizontalCoord: Char): Boolean =
    vertical.contains(verticalCoord) && horizontal.contains(horizontalCoord)

  def forWall(wall: Wall): String = {
    val v = vertical(wall.location.estimateDistance(West))
    val h = horizontal(wall.location.estimateDistance(North))

    wall.orientation match {
      case Vertical => s"$v$h"
      case Horizontal => s"$h$v"
    }
  }
}
