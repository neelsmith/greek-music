package edu.holycross.shot.greekmusic

case class Note(context: MusicalContext, pitch: Int, accidental: Int, rhythm: Int, arsis: Boolean, hyphen: Boolean) {

  def ucode : String = {
     Note.ucodeMap(context.abbr + ":" + pitch + "." + accidental)
  }
}

object Note {
  val ranges = Vector(1 to 24, 1 to 3,1 to 5,0 to 1, 0 to 1)


  /** Determine musical context of a string value for a note.
  *
  * @param s String value to parse.
  *
  */
  def contextForString(s: String): MusicalContext = {
    s match {
      case "gcn:" => CommonNotation
      case "gin:" => InstrumentalNotation
      case "gvn:" => VocalNotation
      case _ => throw new Exception("Unrecognized value for note context: " + s)
    }
  }




  /** Create Note object for a String serialization of a Note value.
  *
  * @param s String to parse.
  */
  def noteFromString(s: String) : Note = {
    val context = contextForString(s.take(4))

    val parts = s.drop(4).split("\\.")
    require (parts.size == 5, "Bad syntax: wrong number of dot-separated parts in " + s)

    val pitch = parts(0).toInt
    val accidental = parts(1).toInt
    val rhythm = parts(2).toInt
    val arsis = (parts(3).toInt == 1)
    val hyphen = (parts(4).toInt == 1)

    if (context == CommonNotation) {
        require(pitch == 0, "Leimma must have value of 0 for pitch")
        require(accidental == 0, "Leimma must have value of 0 for accidental")
    } else {
      for (i <- 0 until parts.size) {
        val r = ranges(i)
        val partsVal = parts(i).toInt
        require(r contains partsVal, "Value " + parts(i) + " out of range for component " + i + s" (legal range: ${ranges(i)})" )
      }
    }

    Note(context, pitch, accidental, rhythm, arsis, hyphen )
  }

  /** Create Note from String.
  */
  def apply(s: String) : Option[Note] = {
    s match {
      case "_" => None
      case note: String => Some(noteFromString(note))
    }
  }


  val ucodeMap = Map(

    "gvn:1.1" -> "𝈀",
    "gvn:1.2" -> "𝈁",
    "gvn:1.3" -> "𝈂",
    "gvn:2.1" -> "𝈃",
    "gvn:2.2" -> "𝈄",
    "gvn:2.3" -> "𝈅",
    "gvn:3.1" -> "𝈆",
    "gvn:3.2" -> "𝈇",
    "gvn:3.3" -> "𝈈",
    "gvn:4.1" -> "𝈉",
    "gvn:4.2" -> "𝈊",
    "gvn:4.3" -> "𝈋",
    "gvn:5.1" -> "𝈌",
    "gvn:5.2" -> "𝈍",
    "gvn:5.3" -> "𝈎",
    "gvn:6.1" -> "𝈏",
    "gvn:6.2" -> "𝈐",
    "gvn:6.3" -> "𝈑",
    "gvn:7.1" -> "𝈒",
    "gvn:7.2" -> "𝈓",
    "gvn:7.3" -> "𝈔",
    "gvn:8.1" -> "𝈕",
    "gvn:8.2" -> "𝈖",
    "gvn:8.3" -> "𝈗",
    "gvn:9.1" -> "Ω",
    "gvn:9.2" -> "Ψ",
    "gvn:9.3" -> "Χ",
    "gvn:10.1" -> "Φ",
    "gvn:10.2" -> "Υ",
    "gvn:10.3" -> "Τ",
    "gvn:11.1" -> "Ϲ",
    "gvn:11.2" -> "Ρ",
    "gvn:11.3" -> "Π",
    "gvn:12.1" -> "Ο",
    "gvn:12.2" -> "Ξ",
    "gvn:12.3" -> "Ν",
    "gvn:13.1" -> "Μ",
    "gvn:13.2" -> "Λ",
    "gvn:13.3" -> "Κ",
    "gvn:14.1" -> "Ι",
    "gvn:14.2" -> "Θ",
    "gvn:14.3" -> "Η",
    "gvn:15.1" -> "Ζ",
    "gvn:15.2" -> "Ε",
    "gvn:15.3" -> "Δ",
    "gvn:16.1" -> "Γ",
    "gvn:16.2" -> "Β",
    "gvn:16.3" -> "Α",
    "gvn:17.1" -> "Ʊ",
    "gvn:17.2" -> "𝈘",
    "gvn:17.3" -> "𝈙",
    "gvn:18.1" -> "𝈚",
    "gvn:18.2" -> "𝈛",
    "gvn:18.3" -> "𝈜",
    "gvn:19.1" -> "Ο′",
    "gvn:19.2" -> "Ξ′",
    "gvn:19.3" -> "Ν′",
    "gvn:20.1" -> "Μ′",
    "gvn:20.2" -> "Λ′",
    "gvn:20.3" -> "Κ′",
    "gvn:21.1" -> "Ι′",
    "gvn:21.2" -> "Θ′",
    "gvn:21.3" -> "Η′",
    "gvn:22.1" -> "Ζ′",
    "gvn:22.2" -> "Ε′",
    "gvn:22.3" -> "Δ′",
    "gvn:23.1" -> "Γ′",
    "gvn:23.2" -> "Β′",
    "gvn:23.3" -> "Α′",
    "gvn:24.1" -> "Ʊ′",
    "gin:1.1" -> "𝈝",
    "gin:1.2" -> "𝈞",
    "gin:1.3" -> "𝈂",
    "gin:2.1" -> "𝈟",
    "gin:2.2" -> "𝈠",
    "gin:2.3" -> "Τ",
    "gin:3.1" -> "𝈡",
    "gin:3.2" -> "𝈢",
    "gin:3.3" -> "𝈆",
    "gin:4.1" -> "Η",
    "gin:4.2" -> "𝈣",
    "gin:4.3" -> "𝈤",
    "gin:5.1" -> "𝈥",
    "gin:5.2" -> "𝈦",
    "gin:5.3" -> "𝈑",
    "gin:6.1" -> "Ε",
    "gin:6.2" -> "𝈧",
    "gin:6.3" -> "𝈨",
    "gin:7.1" -> "𝈩",
    "gin:7.2" -> "𝈜",
    "gin:7.3" -> "𝈅",
    "gin:8.1" -> "Γ",
    "gin:8.2" -> "𝈪",
    "gin:8.3" -> "𝈫",
    "gin:9.1" -> "𝈬",
    "gin:9.2" -> "𝈭",
    "gin:9.3" -> "𝈮",
    "gin:10.1" -> "𝈓",
    "gin:10.2" -> "𝈯",
    "gin:10.3" -> "𝈰",
    "gin:11.1" -> "Ϲ",
    "gin:11.2" -> "𝈱",
    "gin:11.3" -> "Ͻ",
    "gin:12.1" -> "Κ",
    "gin:12.2" -> "𝈎",
    "gin:12.3" -> "𝈲",
    "gin:13.1" -> "𝈳",
    "gin:13.2" -> "𝈴",
    "gin:13.3" -> "𝈵",
    "gin:14.1" -> "𝈶",
    "gin:14.2" -> "𝈍",
    "gin:14.3" -> "𝈷",
    "gin:15.1" -> "𝈸",
    "gin:15.2" -> "𝈈",
    "gin:15.3" -> "𝈹",
    "gin:16.1" -> "Ν",
    "gin:16.2" -> "𝈺",
    "gin:16.3" -> "𝈻",
    "gin:17.1" -> "𝈼",
    "gin:17.2" -> "𝈽",
    "gin:17.3" -> "𝈾",
    "gin:18.1" -> "𝈿",
    "gin:18.2" -> "𝉀",
    "gin:18.3" -> "𝉁",
    "gin:19.1" -> "Κ′",
    "gin:19.2" -> "𝈎′",
    "gin:19.3" -> "𝈲′",
    "gin:20.1" -> "𝈳′",
    "gin:20.2" -> "𝈴′",
    "gin:20.3" -> "𝈵′",
    "gin:21.1" -> "𝈶′",
    "gin:21.2" -> "𝈍′",
    "gin:21.3" -> "𝈷′",
    "gin:22.1" -> "𝈸′",
    "gin:22.2" -> "𝈈′",
    "gin:22.3" -> "𝈹′",
    "gin:23.1" -> "Ν′",
    "gin:23.2" -> "𝈺′",
    "gin:23.3" -> "𝈻′",
    "gin:24.1" -> "𝈼′",
    "gcn:1" -> "𝉅"

  )


}
