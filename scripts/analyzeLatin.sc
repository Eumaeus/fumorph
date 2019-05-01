
import scala.io.Source
import java.io._
import java.util.Calendar
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import scala.io.StdIn.readLine
import scala.collection.mutable._
import edu.furman.classics.fumorph._
import java.util.Calendar

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

val lang = Latin

/*
val lexIdxPath = "jvm/src/test/resources/small_grcIdx.txt"
val morphLibFile:String = "jvm/src/test/resources/very_small_greek.cex"
val textLibrary:String = "jvm/src/test/resources/candaules_short.cex"
val lexiconFile:String = "jvm/src/test/resources/small_lsj_short.cex"
*/

val lexIdxPath = "jvm/src/test/resources/new_latin_index.txt"
val morphLibFile:String = "/Users/cblackwell/Dropbox/CITE/scala/cexshop/morphology/poetry_morphology_lat.cex"
val textLibrary:String = "/Users/cblackwell/Dropbox/CITE/scala/cexshop/cex/aeneid_1_intro.cex"
val lexiconFile:String = "jvm/src/test/resources/ls_short.cex"


val idx:LexIndex = LexIndex(lexIdxPath)
lazy val morphLib = loadLibrary(morphLibFile)
lazy val textLib = loadLibrary(textLibrary)
lazy val lexicon = loadLibrary(lexiconFile)


val t1:Long = Calendar.getInstance().getTimeInMillis()
val mta = MorphTextAligner(
	lang,
	textLib,
	FuMorph(Some(morphLib), textLib, lang),
	lexicon,
	idx
)
val t2:Long = Calendar.getInstance().getTimeInMillis()
println(s"Initialized morph library in: ${(t2 - t1)/1000} seconds.")


lazy val results:Vector[MorphPair] = mta.formsForCorpus

def analyze:Unit = {
	val startTime:Long = Calendar.getInstance().getTimeInMillis()
	println(s"${results.size}")
	val endTime:Long = Calendar.getInstance().getTimeInMillis()
	println(s"Completed in: ${(endTime - startTime)/1000} seconds.")
	println(s"""Results in object "mta".""")
}



