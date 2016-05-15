package minotaur.model

sealed trait Direction {
  val orientation: Orientation = this match {
    case North => Vertical
    case South => Vertical
    case East => Horizontal
    case West => Horizontal
  }
}
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction
