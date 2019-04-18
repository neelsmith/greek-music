package edu.holycross.shot.greekmusic
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.mid.validator._
import org.scalatest.FlatSpec



/**
*/
class EpigraphicVerseLReaderSpec extends FlatSpec {


  "The EpigraphicVerseLReader object" should "recognize an empty gap element" in {
    val s = "<w><gap/>απε<gap/></w>"
    val expected = "[…]απε[…]"
    assert(EpigraphicVerseLReader.diplomatic(s) == expected)
  }

  it should "recognize numbers of missing whole monosemes" in {
    val s = "<gap extent=\"2\" unit=\"monoseme\" /> "
    val expected = " [2 monosemes] "
    assert(EpigraphicVerseLReader.diplomatic(s) == expected)
  }

}
