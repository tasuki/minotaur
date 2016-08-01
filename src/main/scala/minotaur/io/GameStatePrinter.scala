package minotaur.io

import minotaur.model.{GameState,Black,White}

object GameStatePrinter {
  def apply(gs: GameState): Unit = {
    println
    println("Walls left, black: " + gs.walls(Black) + ", white " + gs.walls(White))
    println
    print(BoardPrinter.printWithCoords(gs.board))
  }
}
