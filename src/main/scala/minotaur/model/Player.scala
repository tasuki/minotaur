package minotaur.model

sealed trait Player {
  def pawn: Char
  val next: Player = this match {
    case Black => White
    case White => Black
  }
}
object Player {
  val all: Seq[Player] = Seq(Black, White)
}

case object Black extends Player {
  def pawn = 'x'
}
case object White extends Player {
  def pawn = 'o'
}
