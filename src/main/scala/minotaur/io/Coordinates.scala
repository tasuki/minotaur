package minotaur.io

object Coordinates {
  val vertical   = "abcdfghijklmopr".toList
  val horizontal = "123456789tuvxyz".toList

  def exist(verticalCoord: Char, horizontalCoord: Char): Boolean =
    vertical.contains(verticalCoord) && horizontal.contains(horizontalCoord)
}
