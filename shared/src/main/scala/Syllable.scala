package edu.holycross.shot.greekmusic
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.mid.validator._

case class Syllable(urn: CtsUrn, s: String, notes: Vector[Note]) {
}
//gvn:11.1.1.0.0

object Syllable {

  def apply(syllableToken: MidToken) : Syllable = {
    val noteStrings = syllableToken.string.split("~")
    val notes = for (ns <- noteStrings) yield {
      Note(ns)
    }

    Syllable(syllableToken.urn, syllableToken.string, notes.toVector)
  }

}
