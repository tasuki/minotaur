package minotaur.io

import org.specs2.mutable.Specification

import minotaur.model.{BoardType,Location,Wall,Board}
import minotaur.model.{Horizontal,Vertical}

class BoardReaderSpec extends Specification {
  "An invalid board" should {
    "throw an exception on invalid board shape" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o
        |.   .   .   .
        |
        |.   .   .
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: The board should be square"
      ))
    }

    "throw an exception on non-square board" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o
        |.   .   .   .
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: The board should be square"
      ))
    }
  }


  "Invalid horizontal walls" should {
    "throw an exception on half-wall" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o
        |.   .___.   .
        |
        |.   .   .   .
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Horizontal walls do not form proper pairs"
      ))
    }

    "throw an exception on walls not in the same row" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o
        |.   .___.   .
        |
        |.   .   .___.
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Horizontal walls do not form proper pairs"
      ))
    }

    "throw an exception on disconnected walls" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o
        |.___.   .___.
        |
        |.   .   .   .
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Horizontal walls do not form proper pairs"
      ))
    }
  }


  "Invalid vertical walls" should {
    "throw an exception on half-wall" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o |
        |.   .   .   .
        |
        |.   .   .   .
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Vertical walls do not form proper pairs"
      ))
    }

    "throw an exception on walls not in the same column" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o |
        |.   .   .   .
        |    |
        |.   .   .   .
        |      x
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Vertical walls do not form proper pairs"
      ))
    }

    "throw an exception on disconnected walls" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o |
        |.   .   .   .
        |
        |.   .   .   .
        |      x |
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Vertical walls do not form proper pairs"
      ))
    }
  }

  "Crossing walls" should {
    "throw an exception" in {
      BoardReader.fromString("""
        |.   .   .   .
        |      o
        |.   .   .   .
        |        |
        |.   .___.___.
        |      x |
        |.   .   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: Walls cross each other"
      ))
    }
  }

  "A board with incorrect pawns" should {
    "throw an exception on missing black pawn" in {
      BoardReader.fromString("""
        |.   .   .
        |      o
        |.   .   .
        |
        |.   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: There needs to be exactly one pawn x"
      ))
    }

    "throw an exception on missing white pawn" in {
      BoardReader.fromString("""
        |.   .   .
        |      x
        |.   .   .
        |
        |.   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: There needs to be exactly one pawn o"
      ))
    }

    "throw an exception on double black pawn" in {
      BoardReader.fromString("""
        |.   .   .
        |      o
        |.   .   .
        |  x   x
        |.   .   .
      """.stripMargin.trim) must throwA(new IllegalArgumentException(
        "requirement failed: There needs to be exactly one pawn x"
      ))
    }
  }


  "Valid sample board" should {
    val board = BoardReader.fromFile("src/test/resources/board.txt")
    val boardType = BoardType(9)

    "have correct board type" in {
      board.boardType === boardType
    }

    "have correct walls" in {
      board.walls == Set(
        Wall(Location(19, boardType), Horizontal),
        Wall(Location(48, boardType), Horizontal),
        Wall(Location(55, boardType), Vertical),
        Wall(Location(58, boardType), Vertical)
      )
    }

    "have correctly positioned pawns" in {
      board.black === Location(58, boardType)
      board.white === Location(4, boardType)
    }
  }
}
