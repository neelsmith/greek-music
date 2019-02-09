package edu.holycross.shot.greekmusic


/** A named category of token for a given orthography. */
sealed trait MusicalContext {
  def abbr: String
}

case object VocalNotation extends MusicalContext {
  def abbr = "gvn"
}

case object InstrumentalNotation extends MusicalContext {
  def abbr = "gin"
}

// leimma
case object CommonNotation extends MusicalContext {
  def abbr = "gcn"
}
