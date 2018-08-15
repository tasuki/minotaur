package minotaur.io

import org.specs2.mutable.Specification

class RecommenderTest extends Specification {
  section("integration")

  "Recommender" should {
    "recommend moves" in {
      val record = "e2;e8;e3;f8;e4;d4h;d3h;e3v;d4;b4h;b3h;f7;f6h;g7;c4;h6v;g6v;f7;b4"
      val game = QuoridorStrats.importGame(record)

      val recommender = new Recommender("http://localhost:8008")
      val firstMove = recommender.recommend(game).head
      QuoridorStrats.toQuoridorStrats(firstMove) === "e7"
    }
  }

  section("integration")
}
