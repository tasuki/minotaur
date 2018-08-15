package minotaur.io

import cats.syntax.either._
import com.softwaremill.sttp.quick._
import io.circe.generic.auto._
import io.circe.parser._
import minotaur.model._

class Recommender(
  val recommenderUri: String
) {
  case class RecommendedMove(move: String, prob: Int)

  def recommend(game: Game): Seq[Move] = {
    val gameStr = QuoridorStrats.exportGame(game)
    val body = sttp
      .get(uri"$recommenderUri?game=$gameStr")
      .send()
      .unsafeBody

    val recommended = decode[List[RecommendedMove]](body).getOrElse(Nil)
    val legal = game.state.legalMoves.toSet

    recommended.map {
      case RecommendedMove(mv, pb) =>
        val move = QuoridorStrats.getMove(game.state, mv)
        println(MovePrinter.print(move) + " " + pb.toString)
        move
    } filter legal.contains
  }
}
