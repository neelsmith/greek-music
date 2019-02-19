package edu.holycross.shot.greekmusic
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.mid.validator._
import scala.scalajs.js
import scala.scalajs.js.annotation._

/** An orthographic system for neumes encoded in the
* Virgapes encoding, registered to a specific text or set
* of texts identified by CtsUrn.
*/
@JSExportTopLevel("GreekMusic")  object GreekMusic extends MidOrthography {

  /** Name of orthographic system implementing MidOrthogaphy.*/
  def orthography = "Greek musical notation"


  /** Tab character.*/
  val tab = 0x9
  /** Newline character.*/
  val nl = 0xA
  /** Carriage return character.*/
  val cr = 0xD
  /** Space character.*/
  val space = 0x20
  /** Collection of all whitespace characters.*/
  val whiteSpace = Vector(space, tab, nl, cr)

  /** Period character.*/
  val period = 0x2e
  /** Tilde character.*/
  val tilde = 0x7e
  /** Colon character. */
  val colon = 0x3a
  /** Digits 0 to 9.*/
  val digits = (0x30 to 0x39).toVector

  val alphabetic = Vector(
    'g'.toInt, 'c'.toInt, 'n'.toInt, 'v'.toInt, 'i'.toInt
  )


  /** All valid code points. */
  val cpList:  Vector[Int] =  alphabetic ++ digits :+ space :+ period :+ tilde :+ colon


  /** True if cp is a valid code point in the
  * Virgapes encoding.
  *
  * @param cp Codepoint to check.
  */
  def validCP(cp: Int): Boolean = {
    cpList.contains(cp)
  }

  /** Categories of tokens recognized by this orthography.*/
  def tokenCategories = {
    Vector(SyllableToken)
  }


  /** Tokenization of a citable node of text in GreekMusic orthography
  * into syllabic units.
  *
  * @param n Node to tokenize.
  */
  def tokenizeNode(n: CitableNode): Vector[MidToken] = {


    val urn = n.urn
    val syllables = n.text.split(" ").filter(_.nonEmpty)

    val classified = for (syllable <- syllables.zipWithIndex) yield {
      val newPassage = urn.passageComponent + "." + syllable._2
      val newVersion = urn.addVersion(urn.versionOption.getOrElse("") + "_tkns")
      val newUrn = CtsUrn(newVersion.dropPassage.toString + newPassage)
      val trimmed = syllable._1.trim
      MidToken(newUrn, trimmed, Some(SyllableToken))
    }
    classified.toVector
  }

  def syllablesForTokens(tkns: Vector[MidToken]): Vector[Syllable] = {
    for (tkn <- tkns) yield {
      Syllable(tkn)
    }
  }
}
