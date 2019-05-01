
import scala.io.Source
import java.io._
import java.util.Calendar
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import edu.furman.classics.citewriter._
import scala.io.StdIn.readLine
import scala.collection.mutable._
import edu.furman.classics.fumorph._
import java.util.Calendar


def loadFile(fp:String):Vector[String] = {
	Source.fromFile(fp).getLines.toVector
}

def saveString(s:String, filePath:String , fileName:String):Unit = {
	val pw = new PrintWriter(new File(filePath + fileName))
	for (line <- s.lines){
		pw.append(line)
		pw.append("\n")
	}
	pw.close
}


val corp:Corpus = mta.textRepo.corpus

//val workCorpora:Vector[Corpus] = HtmlWriter.groupCorpusByText(corp)

val corpusHtml:String = {
	val h:String = HtmlWriter.writeCorpus(corp, tokenized = true, Some(textLib.textRepository.get.catalog))
	s"""<div id="text">""" + h + "</div>"
}

val morphHtml:String = {
	s"""
	<div class="fumorph_morphologyRecords">
	${mta.formsForCorpus.map(_.html).mkString("\n")}
	</div>
	"""	
}

val htmlFilePath:String = "/Users/cblackwell/Desktop/"
val htmlFileName:String = "propertius_1_demo.html"
val htmlPageTitle:String = "<i>Carmina</i> 1.1–1.3"
val htmlPageDesc:String = "Hover for a quick view. Click to fix a view; click again to return to hovering. Compressed lexicon entries are linked to the lexicon. Resize the columns by grabbing the divider near the top."

val htmlOpenTemplate:String = loadFile("jvm/src/test/resources/html_1page_template1.html").mkString("\n")
val htmlCloseTemplate:String = loadFile("jvm/src/test/resources/html_1page_template2.html").mkString("\n")
val appCssFile:String = loadFile("/Users/cblackwell/Dropbox/CITE/scala/morphReader/target/scala-2.12/classes/reader.css").mkString("\n")
val jquiCssFile:String = loadFile("/Users/cblackwell/Dropbox/CITE/scala/morphReader/target/scala-2.12/classes/jquery-ui.min.css").mkString("\n")
val jqJsFile:String = loadFile("/Users/cblackwell/Dropbox/CITE/scala/morphReader/target/scala-2.12/classes/jquery-3.4.0.min.js").mkString("\n")
val jquiJsFile:String = loadFile("/Users/cblackwell/Dropbox/CITE/scala/morphReader/target/scala-2.12/classes/jquery-ui.min.js").mkString("\n")
val appJsFile:String = loadFile("/Users/cblackwell/Dropbox/CITE/scala/morphReader/target/scala-2.12/morphreader-opt.js").mkString("\n")

val htmlOpen:String = {
		htmlOpenTemplate.replaceAll("APP-CSS-HERE", appCssFile).replaceAll("JQUI-CSS-HERE", jquiCssFile).replaceAll("PAGE-TITLE-HERE", htmlPageTitle).replaceAll("PAGE-DESC-HERE",htmlPageDesc)
}

println(s"\n got here \n")

val htmlClose:String = {
	//htmlCloseTemplate
		val s1:String = htmlCloseTemplate.replaceAllLiterally("JQ-JS-HERE",jqJsFile)
		val s2:String = s1.replaceAllLiterally("JQUI-JS-HERE", jquiJsFile)
println(s"\n got here s2\n")
		val s3:String = s2.replaceAllLiterally("APP-JS-HERE", appJsFile)
println(s"\n got here s3\n")
		s3
}


println(s"\n got here \n")


val html:String = {
	Vector(htmlOpen,morphHtml, corpusHtml, htmlClose).mkString("\n\n")
}

saveString(html, htmlFilePath, htmlFileName)