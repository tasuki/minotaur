package minotaur.io

import org.specs2.mutable.Specification

import minotaur.model.{GameState,Black,White}

class GameStatePrinterSpec extends Specification {
  "State from sample board" should {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 3, White -> 10), Black
    )

    "get printed properly" in {
      GameStatePrinter.succinct(gs) === "x:58:3,o:4:10,1i2a3b6d7a7i8a:a1i2b7f7i8"
    }
  }
}
