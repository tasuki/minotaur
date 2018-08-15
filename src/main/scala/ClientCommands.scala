import minotaur.io.{ GameStatePrinter, MovePrinter, NodePrinter, Recommender }
import minotaur.mcts.MCTS
import minotaur.model.{ Game, Move }
import profile.Profiler

sealed trait Command {
  def execute(game: Game): Game
}

case object Quit extends Command {
  def execute(game: Game): Game = {
    println("You have escaped the labyrinth.")
    System.exit(0)
    game
  }
}

case object Undo extends Command {
  def execute(game: Game): Game = {
    val undoed = game.parent.flatMap(_.parent).getOrElse(game)
    print(GameStatePrinter(undoed.state))
    undoed
  }
}

case class Play(move: Move, mcts: MCTS, recommender: Recommender) extends Command {
  private def findMcts(game: Game): Game = {
    println
    println("Minotaur is feeding on the dead bodies of his victims, please wait...")

    val node = Profiler.profile("MCTS", mcts.findMove(game.state))
    Profiler.printComplete()
    Profiler.clear()
    node.parent.map { node =>
      NodePrinter.printTopMoves(node).map(println)
    }

    if (node.winRatio < 0.1) {
      println
      println("Congratulations, Theseus, you have killed the Minotaur!")
      System.exit(0)
    }

    val gameAfterAImove = Game(node.gameState, node.move, Some(game))
    print(GameStatePrinter(gameAfterAImove.state))

    if (node.wins) {
      println
      println("You have been devoured by the Minotaur. RIP")
      System.exit(0)
    }

    gameAfterAImove
  }

  def findRecommended(game: Game): Game = {
    val moves = recommender.recommend(game)

    val move = moves.head
    val gameAfterAImove = Game(move.play, Some(move), Some(game))
    print(GameStatePrinter(gameAfterAImove.state))

    if (move.wins) {
      println
      println("You have been devoured by the Minotaur. RIP")
      System.exit(0)
    }

    gameAfterAImove
  }

  def execute(game: Game): Game = {
    if (! game.state.legalMoves.contains(move)) {
      println("That move is illegal, try again")
      return game
    }

    val gameAfterPlayerMove = Game(move.play, Some(move), Some(game))
    print(GameStatePrinter(gameAfterPlayerMove.state))

    if (move.wins) {
      println
      println("Congratulations, Theseus, you have killed the Minotaur!")
      System.exit(0)
    }

    findRecommended(gameAfterPlayerMove)
    //findMcts(gameAfterPlayerMove)
  }
}

case object Unknown extends Command {
  def execute(game: Game): Game = {
    println("Sorry, didn't understand that")
    game
  }
}
