package minotaur.mcts

import org.specs2.mutable.Specification
import minotaur.io._
import minotaur.mcts._
import minotaur.model._

class TestSpec extends Specification {
  section("wip")

  "For a side, MCTS" should {
    val file = "src/test/resources/problems/side.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 7, White -> 8), Black
    )

    "choose a move" in {
      val node = new MCTS(1000000).findMove(gs)

      println
      println(BoardPrinter.print(node.move.get.play.board))
      Debug.printFullInfo(node.parent.get)
      //profile.Profiler.printComplete

      node.move === Some(WallPlacement(
        Wall(Location(40, board.boardType), Horizontal),
        gs
      ))
      ok
    }
  }


  "Play a full game" should {
    val file = "src/test/resources/empty.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 10, White -> 10), Black
    )

    "play a full game" in {
      // play full game
      var state = gs
      var mn: Node = null
      val mcts = new MCTS(30000)
      mn = mcts.findMove(state)

      while (mn.wins == false) {
        state = mn.gameState
        println
        print(mn)
        print(GameStatePrinter(state))
        mn = mcts.findMove(state, mn.toRoot)
      }

      profile.Profiler.printComplete

      ok
    }
  }

  "For a tricky board, MCTS" should {
    val file = "src/test/resources/problems/tricky.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 5, White -> 3), Black
    )

    "choose a move" in {
      val node = new MCTS(1000000).findMove(gs)

      println
      println(BoardPrinter.print(node.gameState.board))
      Debug.printFullInfo(node.parent.get)

      ok
    }

    "play a full game" in {
      var state = gs
      var node: Node = null
      do {
        node = new MCTS().findMove(state)
        state = node.move.get.play
        println(BoardPrinter.print(state.board))
      } while (node.move.get.wins == false)

      ok
    }
  }

  "Hidden costs" should {
    val file = "src/test/resources/problems/hidden-costs.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 0, White -> 3), White
    )

    "should choose a move for white" in {
      val node = new MCTS(10000).findMove(gs)
      Debug.printFullInfo(node.parent.get)

      ok
    }

    "play a full game" in {
      var state = gs
      var node: Node = null

      do {
        node = new MCTS(10000).findMove(state)
        state = node.move.get.play
        Debug.printShortInfo(node)
      } while (node.move.get.wins == false)

      ok
    }
  }

  section("wip")
}
