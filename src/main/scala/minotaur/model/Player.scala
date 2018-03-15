package minotaur.model

sealed trait Player {
  val pawn: Char
  val destination: Direction
  val other: Player = this match {
    case Black => White
    case White => Black
  }
}
object Player {
  val all: Seq[Player] = Seq(Black, White)
}

case object Black extends Player {
  val pawn: Char = 'x'
  val destination: Direction = North
}
case object White extends Player {
  val pawn: Char = 'o'
  val destination: Direction = South
}
