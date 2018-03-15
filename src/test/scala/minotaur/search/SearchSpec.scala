package minotaur.search

import scala.io.Source

import minotaur.io.BoardReader
import minotaur.model._
import org.specs2.mutable.Specification

class SearchSpec extends Specification {
  val file = "src/test/resources/board.txt"
  val board = BoardReader.fromFile(file)

  def blackNorth(search: Search) = {
    search.findPath(board, board.pawnLocation(Black), North) === Some(
      ShortestPath(Seq(
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
    )
  }

  def whiteSouth(search: Search) = {
    search.findPath(board, board.pawnLocation(White), South) === Some(
      ShortestPath(Seq(
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
    )
  }

  def noPath(search: Search) =
    search.findPath(board, Location(63, board.boardType), North) === None

  "AStar" should {
    val search = AStar
    "find the shortest path north for black" in {
      blackNorth(search)
    }
    "find the shortest path south for white" in {
      whiteSouth(search)
    }
    "find no path from an enclosed space" in {
      noPath(search)
    }
  }

  "BFS" should {
    val search = BFS
    "find the shortest path north for black" in {
      blackNorth(search)
    }
    "find the shortest path south for white" in {
      whiteSouth(search)
    }
    "find no path from an enclosed space" in {
      noPath(search)
    }
  }

  "Flood search" should {
    val search = Flood
    "find everything" in {
      val boardFile = "src/test/resources/floodsearch/board.txt"
      val board = BoardReader.fromFile(boardFile)

      val searchNodes = Flood.findNodes(board, board.pawnLocation(Black))

      val solutionFile = "src/test/resources/floodsearch/solution.txt"
      val solution = Source.fromFile(solutionFile).mkString

      minotaur.io.BoardPrinter.printSearchNodes(board, searchNodes) === solution
    }
  }
}
