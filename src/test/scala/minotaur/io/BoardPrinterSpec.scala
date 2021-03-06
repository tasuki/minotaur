package minotaur.io

import scala.io.Source

import org.specs2.mutable.Specification

class BoardPrinterSpec extends Specification {
  "Sample board" should {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)

    "get printed properly" in {
      BoardPrinter.print(board) === "\n" + Source.fromFile(file).mkString
    }

    "get printed with coordinates" in {
      BoardPrinter.printWithCoords(board) === "\n" + Source.fromFile(
        "src/test/resources/board-coords.txt"
      ).mkString
    }
  }
}
