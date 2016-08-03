import scala.collection.mutable.Stack
import minotaur.io.GameStatePrinter
import minotaur.mcts.MCTS
import minotaur.model.GameState
import minotaur.model.Move
import profile.Profiler

sealed trait Command {
  def execute(game: Stack[GameState]): Boolean // AI on turn
}

case object Quit extends Command {
  def execute(game: Stack[GameState]) = {
    println("You have escaped the labyrinth.")
    System.exit(0)
    false
  }
}

case object Undo extends Command {
  def execute(game: Stack[GameState]) = {
    game.pop
    game.pop
    GameStatePrinter(game.head)
    false
  }
}

case class Play(move: Move, playouts: Int) extends Command {
  def execute(game: Stack[GameState]): Boolean = {
    if (! game.head.getPossibleMoves.contains(move) || ! move.isValid) {
      println("That move is illegal, try again")
      return false
    }

    game.push(move.play)
    GameStatePrinter(game.head)

    if (move.wins) {
      println
      println("Congratulations, Theseus, you have killed the Minotaur!")
      System.exit(0)
    }

    println
    println("Minotaur is feeding on the dead bodies of his victims, please wait...")

    val node = Profiler.profile("MCTS", MCTS.findMove(game.head, playouts))
    Profiler.print("MCTS")
    Profiler.clear
    println(node)

    if (node.winRatio < 0.1) {
      println
      println("Congratulations, Theseus, you have killed the Minotaur!")
      System.exit(0)
    }

    game.push(node.move.play)
    GameStatePrinter(game.head)

    if (node.move.wins) {
      println
      println("You have been devoured by the Minotaur. RIP")
      System.exit(0)
    }

    true
  }
}

case object Unknown extends Command {
  def execute(game: Stack[GameState]) = {
    println("Sorry, didn't understand that")
    false
  }
}
