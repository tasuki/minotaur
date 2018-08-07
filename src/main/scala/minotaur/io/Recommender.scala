package minotaur.io

import cats.syntax.either._
import com.softwaremill.sttp.quick._
import io.circe.generic.auto._
import io.circe.parser._
import minotaur.model._

object Recommender {
  case class RecommendedMove(move: String, prob: Int)

  def recommend(game: Game): Seq[Move] = {
    val gameStr = QuoridorStrats.exportGame(game)
    val body = sttp
      .get(uri"http://localhost:8008?game=$gameStr")
      .send()
      .unsafeBody

    val recommended = decode[List[RecommendedMove]](body).getOrElse(Nil)
    val legal = game.state.legalMoves.toSet

    recommended.map {
      case RecommendedMove(mv, _) => QuoridorStrats.getMove(game.state, mv)
    } filter legal.contains
  }
}
