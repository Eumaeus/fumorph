import scala.io.Source
import java.io._
import java.util.Calendar
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import edu.holycross.shot.citerelation._
import scala.io.StdIn.readLine
import scala.collection.mutable._
import edu.furman.classics.fumorph._

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

val punctuation:String = """[\[\])(·⸁.,·;;   "?·!–—⸂⸃]"""

def normalizeGreek(s:String):String = {
		// remove punctuation
		val puncStr:String = s.replaceAll(punctuation,"")
		// fix elision
		val elided:Boolean = {
			if (puncStr.size < 1) {
				false
			} else {
				puncStr.last.toString.matches("['ʼ‘’’]")
			}
		}
		val emendedStr:String = {
			if (elided) {
				LiteraryGreekString(puncStr).ucode.dropRight(1).replaceAll("#","") + "'"
			} else {
				puncStr
			}
		}
		// make unicode
		val acuteString:String = {
			if (emendedStr.size < 1) { "" }
			else { LiteraryGreekString(emendedStr).ascii.replaceAll("\\\\","/") }
		}
		val sigmaString:String = {
			if (acuteString.size < 1) { "" }
			else { LiteraryGreekString(acuteString).ucode }
		}

		val finalString:String = {
			if (sigmaString.size < 1) { "" }
			else { 
				val s:String = LiteraryGreekString(sigmaString).ucode 
				if (s.last.toString == "σ") {
					s.dropRight(1) + "ς"
				} else { s }
			}
		}
		finalString
	}



val morphLib = loadLibrary("cex/fullText.cex")
val cr = morphLib.collectionRepository.get
val tr = morphLib.textRepository.get
val crs = morphLib.relationSet.get

val wordU = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.allen.token:1.2.6")
val thisWord = tr.corpus.nodes.filter(_.urn == wordU).head.text
val lexes = crs.urn1Match(wordU)
val morphs = cr.