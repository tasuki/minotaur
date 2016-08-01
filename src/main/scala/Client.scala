import minotaur.io._

import minotaur.model.{GameState,Black,White}
import minotaur.model.{Direction,North,South,East,West}
import minotaur.model.{Location,Wall}
import minotaur.model.{Orientation,Vertical,Horizontal}
import minotaur.model.{Move,PawnMovement,WallPlacement}
import minotaur.io.Coordinates
import minotaur.mcts.MCTS

object Client {
  def main(args: Array[String]): Unit = {
    val cli: Map[String, String] = args.map(
      _.split("=") match { case Array(k, v) => k -> v }
    ).toMap
    val handicap: Int = cli.getOrElse("handicap", "0").toInt

    val player = Black
    val computer = White

    val file = "src/test/resources/empty.txt"
    var gs = GameState(
      BoardReader.fromFile(file), Map(
        computer -> (10 - handicap),
        player -> (10 + handicap)
      ), player
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
        nn: jumps over enemy piece North
        nw: jumps over enemy piece North-West
        a1: places a vertical wall in the upper left corner
        1a: places a horizontal wall in the upper left corner
        i8: places a vertical wall in the bottom right corner
        8i: places a horizontal wall in the bottom right corner
"""
    )

    println
    print(BoardPrinter.printWithCoords(gs.board))

    val coordinates = Coordinates(gs.board.boardType)
    val movePattern = "^([nsew]{1,2})$".r
    val coordsPattern = "^(..)$".r

    while (true) {
      val command: Option[Move] = readLine("Your move: ") match {
        case ("quit" | "exit") => return
        case movePattern(directions) =>
          directions.toList.foldLeft(Option(gs.board.pawnLocation(player)))(
            (optLoc, dir) => optLoc match {
              case Some(loc) => loc.neighbor(Direction.fromChar(dir))
              case _ => None
            }
          ).map(loc => PawnMovement(loc, gs))

        case coordsPattern(coords) => {
          (coords.toList match {
            case List(vertical, horizontal)
                      if (coordinates.exist(vertical, horizontal)) =>
              Some((vertical, horizontal, Vertical))
            case List(horizontal, vertical)
                      if (coordinates.exist(vertical, horizontal)) =>
              Some((vertical, horizontal, Horizontal))
            case _ =>
              None
          }).map((data) => {
            val (vertical, horizontal, orientation) = data

            Wall(
              Location(
                coordinates.vertical.indexOf(vertical) +
                coordinates.horizontal.indexOf(horizontal) * gs.board.size,
                gs.board.boardType
              ),
              orientation
            )
          }).map(wall => WallPlacement(wall, gs))
        }

        case _ => None
      }

      command match {
        case Some(move) if (gs.getPossibleMoves.contains(move) && move.isValid) => {
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
