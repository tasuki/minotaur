package minotaur.model

sealed trait Player {
  val pawn: Char
  val destination: Direction
  val next: Player = this match {
    case Black => White
    case White => Black
  }
}
object Player {
  val all: Seq[Player] = Seq(Black, White)
}

case object Black extends Player {
  val pawn = 'x'
  val destination = North
}
case object White extends Player {
  val pawn = 'o'
  val destination = South
}
