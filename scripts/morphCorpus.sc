
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

val filePath:String = "cex/" 
val splitters:String = """[\[\])(·⸁.,·;;   "?·!–—⸂⸃]"""

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String = "../iliad_alignment/iliad_pope.txt"):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}


def saveString(s:String, filePath:String = filePath, fileName:String = "temp.txt"):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}

def showMe(v:Any):Unit = {
	v match {
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

val libPath = "cex/little.cex"
val lib = loadLibrary(libPath)

val greekTexts:Vector[CtsUrn] = {
	lib.textRepository match {
		case Some(tr) => {
			tr.catalog.texts.filter(_.lang == "grc").map(_.urn).filter(_.isExemplar)
		}
		case None => Vector[CtsUrn]()
	}
}
val latinTexts:Vector[CtsUrn] = {
	lib.textRepository match {
		case Some(tr) => {
			tr.catalog.texts.filter(_.lang == "lat").map(_.urn).filter(_.isExemplar)
		}
		case None => Vector[CtsUrn]()
	}
}

def getPersEntries(texts:Vector[CtsUrn], lang:MorphLanguage):Vector[PersEntry] = {
	lib.textRepository match {
		case Some(tr) => {
			val newCorp:Corpus = {
				val langNodes:Vector[CitableNode] = {
					texts.map( n => {
						(tr.corpus ~~ n).nodes
					}).flatten
				}
				Corpus(langNodes)
			}
			PerseusParser.analyzeText(newCorp, lang)
		}
		case None => Vector[PersEntry]()
	}
}

lazy val greekPMs:Vector[PersEntry] = getPersEntries(greekTexts, Greek)
lazy val greekFuMs:Vector[Form] = PerseusParser.toForms(greekPMs)
lazy val grcFu:FuMorph = new FuMorph(Greek)
lazy val greekCex:String = grcFu.cex(Greek, greekFuMs, "fumorph_grc", "test", "#")

saveString(greekCex, fileName = "greekMorph.cex")

lazy val latinPMs:Vector[PersEntry] = getPersEntries(latinTexts, Latin)
lazy val latinFuMs:Vector[Form] = PerseusParser.toForms(latinPMs)
lazy val latFu:FuMorph = new FuMorph(Latin)
lazy val latinCex:String = latFu.cex(Latin, latinFuMs, "fumorph_lat", "test", "#")

saveString(latinCex, fileName = "latinMorph.cex")
