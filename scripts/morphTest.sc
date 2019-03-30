
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
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

val morphModel = Cite2Urn("urn:cite2:cite:datamodels.v1:fumorph")
val lgsModel = Cite2Urn("urn:cite2:cite:datamodels.v1:lgs")

val greekMorphCex = "cex/greekMorph.cex"
val latinMorphCex = "cex/latinMorph.cex"
val textCex = "cex/little.cex"

lazy val textLib = loadLibrary(textCex)
lazy val greekMorph = loadLibrary(greekMorphCex)
lazy val latinMorph = loadLibrary(latinMorphCex)

/*
lazy val cr = lib.collectionRepository.get

lazy val morphCollections:Vector[Cite2Urn] = lib.collectionsForModel(morphModel)

lazy val surfaceFormProperties:Vector[Cite2Urn] = {
	val sfStr:String = "surfaceform"
	morphCollections.map( _.addProperty(sfStr))
}
lazy val lemmaProperties:Vector[Cite2Urn] = {
	val lemStr:String = "lemma"
	morphCollections.map( _.addProperty(lemStr))
}
lazy val lgsProperties:Vector[Cite2Urn] = lib.collectionsForModel(lgsModel)
*/
