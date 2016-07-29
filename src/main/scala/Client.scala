import minotaur.io._

import scala.util.matching.Regex
import minotaur.model.{GameState,Black,White}
import minotaur.model.{North,South,East,West}
import minotaur.model.{Location,Wall}
import minotaur.model.{Orientation,Vertical,Horizontal}
import minotaur.model.{Move,PawnMovement,WallPlacement}
import minotaur.io.Coordinates
import minotaur.mcts.MCTS

object Client {
  def main(args: Array[String]): Unit = {
    val player = Black
    val computer = White

    val file = "src/test/resources/empty.txt"
    var gs = GameState(
      BoardReader.fromFile(file), Map(Black -> 10, White -> 10), player
    )

    print(
      """
      --------------------------------
        Interactive Minotaur console
      --------------------------------

      Example commands:
        quit: exists the console
        n: moves your pawn North
        s: moves your pawn South
        e: moves your pawn East
        w: moves your pawn West
        a1: places a vertical wall in the upper left corner
        1a: places a horizontal wall in the upper left corner
        i8: places a vertical wall in the bottom right corner
        8i: places a horizontal wall in the bottom right corner
"""
    )

    println
    print(BoardPrinter.printWithCoords(gs.board))

    while (true) {
      val pattern = "^(..)$".r
      val command: Option[Move] = readLine("Your move: ") match {
        case ("quit" | "exit") => return
        case d @ ("n" | "s" | "e" | "w") =>
          gs.board.pawnLocation(player).neighbor(d match {
            case "n" => North
            case "s" => South
            case "e" => East
            case "w" => West
          }).flatMap(loc => Some(PawnMovement(loc, gs)))

        case pattern(coords) => {
          (coords.toList match {
            case List(vertical, horizontal)
                      if (Coordinates.exist(vertical, horizontal)) =>
              Some(vertical, horizontal, Vertical)
            case List(horizontal, vertical)
                      if (Coordinates.exist(vertical, horizontal)) =>
              Some(vertical, horizontal, Horizontal)
            case _ =>
              None
          }).flatMap((data) => {
            val (vertical, horizontal, orientation) = data

            Some(Wall(
              Location(
                Coordinates.vertical.indexOf(vertical) +
                Coordinates.horizontal.indexOf(horizontal) * gs.board.size,
                gs.board.boardType
              ),
              orientation
            ))
          }).flatMap(wall => Some(WallPlacement(wall, gs)))
        }

        case _ => None
      }

      command match {
        case Some(move) if (move.isValid) => {
          gs = move.play
          println
          print(BoardPrinter.printWithCoords(gs.board))
          if (move.wins) {
            println
            println("Congratulations, Theseus, you have killed the Minotaur!")
            return
          }

          println
          println("Minotaur is feeding on the dead bodies of his victims, please wait...")
          val newMove = MCTS.findMove(gs, 50000).move

          gs = newMove.play
          println
          print(BoardPrinter.printWithCoords(gs.board))
          if (newMove.wins) {
            println
            println("You have been devoured by the Minotaur. RIP")
            return
          }

          println
          println("Walls left, you: " + gs.walls(player) + ", minotaur " + gs.walls(computer))
          println
        }
        case Some(move) => println("That move is illegal, try again")
        case _ => println("Sorry, didn't understand that")
      }
    }
  }
}
