## Minotaur

[![Build Status](https://travis-ci.org/tasuki/minotaur.png?branch=master)](https://travis-ci.org/tasuki/minotaur)

Minotaur is an MCTS-based AI to play [Quoridor]. The author can usually beat it.

To play against your computer, you'll need to install [sbt]. Then open the
console, go to the directory containing this readme, and run:

    sbt "run-main Client"

### Development

Run the tests with:

    sbt 'testOnly * -- exclude integration,wip'

Run MCTS tests (may fail somewhat randomly):

    sbt 'testOnly *MCTSSpec'

Run WIP tests (_will_ fail):

    sbt 'testOnly *TestSpec'
---

_When he had grown up and become a most ferocious animal, and of incredible
strength, they tell that Minos had him shut up in a prison called the
labyrinth, and that he had sent to him there all those whom he wanted to die a
cruel death._

[Quoridor]: https://en.wikipedia.org/wiki/Quoridor
[sbt]: http://www.scala-sbt.org/
