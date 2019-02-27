package edu.holycross.shot.greekmusic
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.mid.validator._
import org.scalatest.FlatSpec



/**
*/
class NoteSpec extends FlatSpec {


  "A Note" should "require recognize a leimma as common notation" in  {
    val validLeimma = Note("gcn:0.0.1.1.1")
    assert (validLeimma.context == CommonNotation)
  }


  it should "produce a Unicode string for a Note object" in {
    val note = Note("gvn:1.1.1.0.0")
    val expected = "𝈀"
    assert(note.ucode == expected)
  }

  it should "reject a Leimma with pitch value" in {
    try {
      val badLeimma = Note("gcn:1.0.1.1.1")
      fail("Should not have created note with pitch for leimma")
    } catch {
      case iae: java.lang.IllegalArgumentException => assert(true)
      case t: Throwable => {fail("Should have thrown IllegalArgumentExcpetion, but got " + t)}
    }
  }

  it should "reject a Leimma with a value for accidental" in  {
    try {
      val badLeimma = Note("gcn:0.1.1.1.1")
      fail("Should not have created note with pitch for leimma")
    } catch {
      case iae: java.lang.IllegalArgumentException => assert(true)
      case t: Throwable => {fail("Should have thrown IllegalArgumentExcpetion, but got " + t)}
    }
  }  /*{
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

  it should "ceate a tokenized  corpus" in {
    val corpus = Corpus(Vector(seikilos1))
    val tokenized = GreekMusic.tokenizedCorpus(corpus)
    val expectedSyllables = 5
    assert(tokenized.size == expectedSyllables)
  }
  U*/
}
