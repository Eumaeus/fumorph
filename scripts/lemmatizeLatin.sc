
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

def collapseNodes(c:Corpus, exemplar:String = "_merged"):Corpus = {
	// check for validity
	val isOkay:Boolean = {
		val uniformDepth:Boolean = {
			c.nodes.map( _.urn.citationDepth).distinct.size == 1
		}
		val deeperThanOne:Boolean = {
			c.nodes.head.urn.citationDepth.head > 1
		}
		uniformDepth & deeperThanOne
	}	
	if (isOkay == false) throw new Exception(s"Nodes needs to be at the same citation depth, and that depth must be > 1.")
		
	/* Example of how to use groupBy in Scala without losing object order */
	val grouped:Vector[(CtsUrn, Vector[(CitableNode,Int)])] = {
		c.nodes.zipWithIndex.groupBy(_._1.urn.collapsePassageBy(1)).toVector
	}
	val sorted1:Vector[(CtsUrn, Vector[(CitableNode,Int)])] = {
		grouped.map( g => {
			val u = g._1
			val v = g._2.sortBy(_._2)
			(u, v)
		})
	}
	val sorted2:Vector[(CtsUrn, Vector[(CitableNode,Int)])] = {
		grouped.sortBy(_._2.head._2)
	}
	
	val newNodes:Vector[CitableNode] = {
		sorted2.map( g => {
			val exemp:String = g._1.exemplar + exemplar
			val u:CtsUrn = g._1.dropExemplar.addExemplar(exemp)
			val t:String = g._2.map(_._1.text).mkString(" ")
			CitableNode(u,t)
		})
	}
	Corpus(newNodes)	
}

def showMe(v:Any):Unit = {
	v match {
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"))
	library
}

def saveString(s:String, fileName:String):Unit = {
	val pw = new PrintWriter(new File(fileName))
		pw.append(s)
		pw.append("\n")
	pw.close
}

val lang = Latin

/*
val lexIdxPath = "jvm/src/test/resources/small_grcIdx.txt"
val morphLibFile:String = "jvm/src/test/resources/very_small_greek.cex"
val textLibrary:String = "jvm/src/test/resources/candaules_short.cex"
val lexiconFile:String = "jvm/src/test/resources/small_lsj_short.cex"
*/

val lexIdxPath = "jvm/src/test/resources/new_latin_index.txt"
val morphLibFile:String = "/Users/cblackwell/Dropbox/CITE/scala/cexshop/morphology/lat_morphology.cex"
val textLibrary:String = "/Users/cblackwell/Dropbox/CITE/scala/cexshop/cex/catullus_64.cex"
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
}
