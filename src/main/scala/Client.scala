import scala.collection.mutable.Stack
import minotaur.io.{BoardReader,GameStatePrinter,Coordinates}
import minotaur.model.{Black,White}
import minotaur.model.{GameState,Location,Direction,Wall}
import minotaur.model.{Vertical,Horizontal}
import minotaur.model.{PawnMovement,WallPlacement}

object Client {
  val player = Black
  val computer = White

  def main(args: Array[String]): Unit = {
    val cli: Map[String, String] = args.map(
      _.split("=") match { case Array(k, v) => k -> v }
    ).toMap
    val handicap: Int = cli.getOrElse("handicap", "0").toInt
    val playouts: Int = cli.getOrElse("playouts", "50000").toInt

    val file = "src/test/resources/empty.txt"
    val game = Stack(GameState(
      BoardReader.fromFile(file), Map(
        computer -> (10 - handicap),
        player -> (10 + handicap)
      ), player
    ))

    print(
      """
      --------------------------------
        Interactive Minotaur console
      --------------------------------

      Example commands:
        quit: exists the console
        undo: undo the last move
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

    GameStatePrinter(game.head)

    while (true) {
      getCommand(game, playouts).execute(game)
    }
  }

  private def getCommand(game: Stack[GameState], playouts: Int): Command = {
    val coordinates = Coordinates(game.head.board.boardType)
    val movePattern = "^([nsew]{1,2})$".r
    val coordsPattern = "^(..)$".r

    readLine("Your move: ") match {
      case "quit" | "exit" => Quit
      case "undo" => Undo

      case movePattern(directions) =>
        directions.toList.foldLeft(Option(game.head.board.pawnLocation(player)))(
          (optLoc, dir) => optLoc match {
            case Some(loc) => loc.neighbor(Direction.fromChar(dir))
            case _ => None
          }
        ).map(loc => Play(PawnMovement(loc, game.head), playouts))
        .getOrElse(Unknown)

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
              coordinates.horizontal.indexOf(horizontal) * game.head.board.size,
              game.head.board.boardType
            ),
            orientation
          )
        }).map(wall => Play(WallPlacement(wall, game.head), playouts))
        .getOrElse(Unknown)
      }

      case _ => Unknown
    }
  }
}
