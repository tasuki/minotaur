package minotaur.model

import minotaur.io.BoardReader
import org.specs2.mutable.Specification

class PathSpec extends Specification {
  val file = "src/test/resources/board.txt"
  val board = BoardReader.fromFile(file)

  "Valid path" should {
    val path = PotentialShortestPath(Seq(
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

    "be identified as such" in {
      path.isValid(Black, board) === true
    }

    "progress forward" in {
      path.forward === ShortestPath(Seq(
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
  }

  "Invalid path" should {
    val path = PotentialShortestPath(Seq(
      Location(57, board.boardType),
      Location(56, board.boardType),
      Location(47, board.boardType),
      Location(38, board.boardType),
      Location(29, board.boardType),
      Location(20, board.boardType),
      Location(11, board.boardType),
      Location(2, board.boardType)
    ))

    "be identified as such" in {
      path.isValid(Black, board) === false
    }
  }

  "Path blocked at the last step" should {
    val board = BoardReader.fromString("""
      |+   +   +   +   +   +   +   +   +   +
      |    |   |   |   |   |   |
      |+   +   +   +   +   +   +   +---+---+
      |    |   |   |   |   |   |       |
      |+---+---+   +---+---+   +   +   +   +
      |                        |       |
      |+   +---+---+---+---+   +   +   +   +
      |                    |   |
      |+---+---+   +   +   +   +   +   +   +
      |            |       |   |
      |+   +   +   +   +   +   +   +   +   +
      |            |         o |       |
      |+---+---+   +---+---+   +   +   +   +
      |        |     x     |   |       |
      |+---+---+   +---+---+   +   +---+---+
      |        |           |   |       |
      |+---+---+---+---+---+---+   +   +   +
      |                                |
      |+   +   +   +   +   +   +   +   +   +
    """.stripMargin.trim)

    val path = ShortestPath(Seq(
      Location(59, board.boardType),
      Location(68, board.boardType),
      Location(77, board.boardType)
    ))

    "be considered invalid" in {
      path.isValid(White, board) === false
    }
  }
}
