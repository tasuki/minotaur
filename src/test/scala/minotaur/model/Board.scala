package minotaur.model

import org.specs2.mutable.Specification
import minotaur.io.BoardReader

class BoardSpec extends Specification {
  "Sample board" should {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)

    "not allow white moving north off the board" in {
      board.canMove(board.white, North) === false
    }
    "allow white moving south to a free space" in {
      board.canMove(board.white, South) === true
    }
    "allow white moving east to a free space" in {
      board.canMove(board.white, East) === true
    }
    "allow white moving west to a free space" in {
      board.canMove(board.white, West) === true
    }

    "not allow black moving north through a wall" in {
      board.canMove(board.black, North) === false
    }
    "allow black moving south to a free space" in {
      board.canMove(board.black, South) === true
    }
    "not allow black moving east through a wall" in {
      board.canMove(board.black, East) === false
    }
    "allow black moving west to a free space" in {
      board.canMove(board.black, West) === true
    }
  }
}
