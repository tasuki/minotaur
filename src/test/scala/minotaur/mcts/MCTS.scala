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
      val moveNode = MCTS.findMove(gs, 10000)
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

    "should choose one of the two good moves for black" in {
      val moveNode = MCTS.findMove(gs, 50000)
      Set(
        WallPlacement(
          Wall(Location(31, board.boardType), Horizontal),
          gs
        ),
        WallPlacement(
          Wall(Location(40, board.boardType), Horizontal),
          gs
        )
      ) must contain(moveNode.move)
    }
  }

  "For a game that's almost finished, MCTS" should {
    val file = "src/test/resources/problems/almost-finished.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 0, White -> 0), White
    )

    "find the winning move for white very quickly" in {
      val moveNode = MCTS.findMove(gs, 100)
      moveNode.move === PawnMovement(
        Location(80, board.boardType),
        gs
      )
    }
  }

  "For a game that just needs running, MCTS" should {
    val file = "src/test/resources/problems/just-run.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 0, White -> 3), White
    )

    "just run" in {
      val moveNode = MCTS.findMove(gs, 100000)

      moveNode.move === PawnMovement(
        Location(50, board.boardType),
        gs
      )
    }
  }

  section("integration")
}
