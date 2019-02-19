package edu.holycross.shot.greekmusic
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.mid.validator._


import org.scalatest.FlatSpec



/**
*/
class JsAccessSpec extends FlatSpec {


  val seikilos1 = CitableNode(
    CtsUrn("urn:cts:greekMusic:seikilos.1.notation:1"),
    "gvn:11.1.1.0.0 gvn:15.1.2.0.0 gvn:15.1.3.1.0 gvn:13.3.1.0.0~gvn:14.1.1.0.0~gvn:15.1.1.0.0 gvn:14.1.3.1.1"
  )

  "The GreekMusic object" should "tokenize a citable node" in {
    val tkns = GreekMusic.tokenizeNode(seikilos1)
    val expectedSyllables = 5
    assert(tkns.size == expectedSyllables)
    val expectedFirst = "gvn:11.1.1.0.0"
    assert(tkns(0).string == expectedFirst)
  }

  it should "create syllables from MidTokens" in {
    val tkns = GreekMusic.tokenizeNode(seikilos1)
    val sylls = GreekMusic.syllablesForTokens(tkns)
    val expectedSyllables = 5
    assert(sylls.size == expectedSyllables)
    val melismatic = sylls(3)
    val expectedNotes =  3
    assert(melismatic.notes.size == expectedNotes)
  }
}
