package minotaur.model

case class BoardType(size: Int = 9) {
  // used to verify location validity
  val possibleLocations: Vector[Int] = (
    for (field <- 0 to size * size - 1) yield field
  )(collection.breakOut)

  // all locations on the board for jumping around
  val locations: Vector[Location] =
    for (field <- possibleLocations)
      yield Location(field, this)

  // used to verify wall validity
  val possibleWallLocations: Set[Location] = (for {
    location <- locations
    if (!location.isBorder(South)) && (!location.isBorder(East))
  } yield location)(collection.breakOut)

  // possible walls on the board
  val possibleWalls: Set[Wall] = for {
    location <- possibleWallLocations
    orientation <- List(Horizontal, Vertical)
  } yield Wall(location, orientation)

  def containsLocation(location: Int): Boolean =
    possibleLocations contains location
}
