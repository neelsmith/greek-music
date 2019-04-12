package edu.holycross.shot.greekmusic
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.mid.validator._

/** A musical syllable has a text URN, a string value, and a (possibly empty)
* Vector of [[Notes]]s.
*
*/
case class Syllable(urn: CtsUrn, s: String, notes: Vector[Note]) {

  //def melismaRatio = notes.size.toFloat
}
//gvn:11.1.1.0.0


object Syllable {

  /** Construct a syllable object from an MidToken for a single syllable
  *
  * @param syllableToken MidToken for one syllable of musical notation.
  *
   */
  def apply(syllableToken: MidToken) : Syllable = {

    if (syllableToken.string == "_") {
      Syllable(syllableToken.urn, syllableToken.string, Vector.empty[Note])

    } else {
      val noteStrings = syllableToken.string.split("~")
      val noteOptions = for (ns <- noteStrings) yield {
        Note(ns)
      }
      Syllable(syllableToken.urn, syllableToken.string, noteOptions.flatten.toVector)
    }
  }

}
