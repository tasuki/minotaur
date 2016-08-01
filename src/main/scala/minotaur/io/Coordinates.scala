package minotaur.io

import minotaur.model.BoardType

case class Coordinates(boardType: BoardType) {
  val vertical   = getCoords("abcdfghijklmopr")
  val horizontal = getCoords("123456789tuvxyz")

  private def getCoords(possibilities: String): List[Char] =
    possibilities.toList.take(boardType.size - 1)

  def exist(verticalCoord: Char, horizontalCoord: Char): Boolean =
    vertical.contains(verticalCoord) && horizontal.contains(horizontalCoord)
}
