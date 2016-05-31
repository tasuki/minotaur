package minotaur.model

sealed trait Player {
  def pawn: Char
}
object Player {
  def all = Seq(Black, White)
}

case object Black extends Player {
  def pawn = 'x'
}
case object White extends Player {
  def pawn = 'o'
}
