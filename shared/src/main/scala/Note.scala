package edu.holycross.shot.greekmusic

case class Note(context: MusicalContext, pitch: Int, accidental: Int, rhythm: Int, arsis: Boolean, hyphen: Boolean) {
}

object Note {
  val ranges = Vector(1 to 24, 1 to 3,1 to 5,0 to 1, 0 to 1)


  def contextForString(s: String): MusicalContext = {
    s match {
      case "gcn:" => CommonNotation
      case "gin:" => InstrumentalNotation
      case "gvn:" => VocalNotation
      case _ => throw new Exception("Unrecognized value for note context: " + s)
    }
  }


  def apply(s: String) : Note = {
    val context = contextForString(s.take(4))

    val parts = s.drop(4).split("\\.")
    require (parts.size == 5, "Bad syntax: wrong number of dot-separated parts in " + s)
    for (i <- 0 until parts.size) {
      val r = ranges(i)
      val partsVal = parts(i).toInt
      require(r contains partsVal, "Value " + parts(i) + " out of range for component " + i + s" (legal range: ${ranges(i)})" )
    }
    val pitch = parts(0).toInt
    val accidental = parts(1).toInt
    val rhythm = parts(2).toInt
    val arsis = (parts(3).toInt == 1)
    val hyphen = (parts(4).toInt == 1)
    Note(context, pitch, accidental, rhythm, arsis, hyphen )
  }


}
