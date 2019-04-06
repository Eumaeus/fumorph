
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

val filePath:String = "" 
val splitters:String = """[\[\])(·⸁.,·;;   "?·!–—⸂⸃]"""

def loadLibrary(fp:String):CiteLibrary = {
	val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
	library
}

def loadFile(fp:String = "../iliad_alignment/iliad_pope.txt"):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}


def saveString(s:String, fileName:String = "cex/test.cex"):Unit = {
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

val lexIndex = loadFile("cex/greekIdx.txt").filter(_.split("#").size == 2)
//val mlib = loadLibrary("cex/fullTest.cex")
val mlib = loadLibrary("cex/backupCex.cex")
//val mlib = loadLibrary("cex/greekMorph.cex")
val tlib = loadLibrary("cex/candaules.cex")
//val tlib = loadLibrary("cex/elided_greek.cex")

val fum:FuMorph = FuMorph(Some(mlib), tlib, Greek, lexIndex)
//val fum:FuMorph = FuMorph(None, tlib, Greek, lexIndex)


