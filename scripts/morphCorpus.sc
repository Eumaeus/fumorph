
import scala.io.Source
import java.io._
import java.util.Calendar
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import scala.io.StdIn.readLine
import scala.collection.mutable._
import edu.furman.classics.fumorph._


/* Utilities */

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
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

/* Pre-existing morphology libraries for Greek and Latin */

val morphModel = Cite2Urn("urn:cite2:cite:datamodels.v1:fumorph")
val lgsModel = Cite2Urn("urn:cite2:cite:datamodels.v1:lgs")

val archivedGreekFP:String = "cex/greekMorph.cex"
lazy val archivedGreekLib = loadLibrary(archivedGreekFP)

lazy val grcCr = archivedGreekLib.collectionRepository.get
lazy val grcMorphCollections:Vector[Cite2Urn] = archivedGreekLib.collectionsForModel(morphModel)

lazy val grcSurfaceFormProperties:Vector[Cite2Urn] = {
	val sfStr:String = "surfaceform"
	grcMorphCollections.map( _.addProperty(sfStr))
}
lazy val grcLemmaProperties:Vector[Cite2Urn] = {
	val lemStr:String = "lemma"
	grcMorphCollections.map( _.addProperty(lemStr))
}

lazy val lgsProperties:Vector[Cite2Urn] = archivedGreekLib.collectionsForModel(lgsModel)

lazy val greekForms:Vector[LiteraryGreekString] = {
	val props:Vector[CitePropertyValue] = grcSurfaceFormProperties.map( sfp => {
		archivedGreekLib.collectionRepository.get.data.data.filter(_.urn ~~ sfp)
	}).flatten
	val greekStrings:Vector[LiteraryGreekString] = {
		props.map(p => LiteraryGreekString(p.propertyValue.asInstanceOf[String]))	
	}
	greekStrings.distinct
}

lazy val greekLemmata:Vector[LiteraryGreekString] = {
	val props:Vector[CitePropertyValue] = grcLemmaProperties.map( sfp => {
		archivedGreekLib.collectionRepository.get.data.data.filter(_.urn ~~ sfp)
	}).flatten
	val greekStrings:Vector[LiteraryGreekString] = {
		props.map(p => LiteraryGreekString(p.propertyValue.asInstanceOf[String]))	
	}
	greekStrings.distinct
}

val archivedLatinFP:String = "cex/latinMorph.cex"
lazy val archivedLatinLib = loadLibrary(archivedLatinFP)
lazy val latCr = archivedLatinLib.collectionRepository.get
lazy val latMorphCollections:Vector[Cite2Urn] = archivedLatinLib.collectionsForModel(morphModel)

lazy val latSurfaceFormProperties:Vector[Cite2Urn] = {
	val sfStr:String = "surfaceform"
	latMorphCollections.map( _.addProperty(sfStr))
}
lazy val latLemmaProperties:Vector[Cite2Urn] = {
	val lemStr:String = "lemma"
	latMorphCollections.map( _.addProperty(lemStr))
}



lazy val latinForms:Vector[String] = {
	val props:Vector[CitePropertyValue] = latSurfaceFormProperties.map( sfp => {
		archivedLatinLib.collectionRepository.get.data.data.filter(_.urn ~~ sfp)
	}).flatten
	val latinStrings:Vector[String] = {
		props.map(p => p.propertyValue.asInstanceOf[String])
	}
	latinStrings.distinct
}

lazy val latinLemmata:Vector[String] = {
	val props:Vector[CitePropertyValue] = latLemmaProperties.map( sfp => {
		archivedLatinLib.collectionRepository.get.data.data.filter(_.urn ~~ sfp)
	}).flatten
	val latinStrings:Vector[String] = {
		props.map(p => p.propertyValue.asInstanceOf[String])	
	}
	latinStrings.distinct
}


/* ----------------------------------------------------- */

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
					}).flatten.distinct
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
lazy val grcFu:MorphCex = new MorphCex(Greek)
lazy val greekCex:String = grcFu.cex(Greek, greekFuMs, "fumorph_grc", "test", "#")

saveString(greekCex, fileName = "greekMorph.cex")

lazy val latinPMs:Vector[PersEntry] = getPersEntries(latinTexts, Latin)
lazy val latinFuMs:Vector[Form] = PerseusParser.toForms(latinPMs)
lazy val latFu:MorphCex = new MorphCex(Latin)
lazy val latinCex:String = latFu.cex(Latin, latinFuMs, "fumorph_lat", "test", "#")

saveString(latinCex, fileName = "latinMorph.cex")
