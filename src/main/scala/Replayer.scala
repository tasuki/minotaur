import scala.annotation.tailrec
import scala.io.StdIn.readLine
import minotaur.io.{ GameStatePrinter, QuoridorStrats }
import minotaur.model.{ Game, GameState }

object Replayer {
  def main(args: Array[String]): Unit = {
    print(
      """
      ------------
        Replayer
      ------------

      Enter a game record in the quoridorstrats notation.

      Commands:
        quit: exit the console
        Enter: next move
        x: previous move
"""
    )

    val gameRecord = readLine("Quoridorstrats game record: ")
    val game = QuoridorStrats.fromString(gameRecord)
    val gameStates = getGameStates(game, Nil)
    var index = 0
    printState(gameStates(index))

    while (true) {
      readLine("> ") match {
        case "quit" | "exit" =>
          System.exit(0)
        case "x" =>
          index -= 1
          printState(gameStates(index))
        case _ if !gameStates.isDefinedAt(index + 1) =>
          println("That's it!")
          System.exit(0)
        case _ =>
          index += 1
          printState(gameStates(index))
      }
    }
  }

  private def printState(gs: GameState): Unit =
    println(GameStatePrinter(gs))

  @tailrec
  def getGameStates(game: Game, states: List[GameState]): List[GameState] =
    game match {
      case Game(gs, Some(parent)) => getGameStates(parent, gs :: states)
      case Game(gs, None) => gs :: states
    }
}
