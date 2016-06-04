package minotaur.search

import org.specs2.mutable.Specification
import minotaur.io.BoardReader
import minotaur.model.{Location,North,South}
import minotaur.model.{Black,White}

class AStarSpec extends Specification {
  "Sample board" should {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)

    "find shortest path north for black" in {
      AStar.findPath(board, board.pawnLocation(Black), North) === Some(Seq(
        Location(57, board.boardType),
        Location(56, board.boardType),
        Location(47, board.boardType),
        Location(38, board.boardType),
        Location(29, board.boardType),
        Location(30, board.boardType),
        Location(21, board.boardType),
        Location(12, board.boardType),
        Location(3, board.boardType)
      ))
    }

    "find shortest path south for white" in {
      AStar.findPath(board, board.pawnLocation(White), South) === Some(Seq(
        Location(13, board.boardType),
        Location(22, board.boardType),
        Location(31, board.boardType),
        Location(40, board.boardType),
        Location(49, board.boardType),
        Location(50, board.boardType),
        Location(59, board.boardType),
        Location(68, board.boardType),
        Location(77, board.boardType)
      ))
    }

    "find no path from an enclosed space" in {
      AStar.findPath(board, Location(63, board.boardType), North) === None
    }
  }
}
