package edu.holycross.shot.greekmusic
import edu.holycross.shot.mid.validator._

/** A syllable token.*/
case object SyllableToken extends MidTokenCategory {
  def name = "syllable"
}

case object NotelessSyllableToken extends MidTokenCategory {
  def name = "noteless syllable"
}
