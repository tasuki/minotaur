package minotaur.mcts

import org.specs2.mutable.Specification
import minotaur.io.{BoardReader,BoardPrinter}
import minotaur.model._

class MCTSSpec extends Specification {
  section("integration")

  "For a trivial board, MCTS" should {
    val file = "src/test/resources/problems/trivial.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 7, White -> 7), Black
    )

    "choose the obvious move" in {
      val moveNode = MCTS.findMove(gs)
      moveNode.move === WallPlacement(
        Wall(Location(40, board.boardType), Horizontal),
        gs
      )
    }
  }

  "For an easy board, MCTS" should {
    val file = "src/test/resources/problems/easy.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 7, White -> 8), Black
    )

    "should choose the right move for black" in {
      val moveNode = MCTS.findMove(gs)
      moveNode.move === WallPlacement(
        Wall(Location(40, board.boardType), Horizontal),
        gs
      )
    }
  }

  section("integration")
}
