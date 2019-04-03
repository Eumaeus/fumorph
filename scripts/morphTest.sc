
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

val greekMorphCexPath = "cex/greekMorph.cex"
val latinMorphCexPath = "cex/latinMorph.cex"
val textCex = "cex/little.cex"

lazy val textLib = loadLibrary(textCex)
lazy val greekMorphLib = loadLibrary(greekMorphCexPath)
lazy val latinMorphLib = loadLibrary(latinMorphCexPath)


lazy val  greekMorph = new MorphCex(Greek)
lazy val  latinMorph = new MorphCex(Latin)

lazy val  greekMorphObjects:Vector[CitableMorphology] = greekMorph.formsFromCiteObjects(greekMorphLib)
lazy val  latinMorphObjects:Vector[CitableMorphology] = latinMorph.formsFromCiteObjects(latinMorphLib)

lazy val gmos = greekMorphObjects
lazy val lmos = latinMorphObjects

lazy val declinables = greekMorphObjects.filter( gmo => {
	gmo.form.typeName match {
		case "NounForm" => true
		case "PronounForm" => true
		case "AdjectiveForm" => true
		case "ArticleForm" => true
		case "ParticipleForm" => true
		case _ => false
	}
}).map( d => { 
	(d.urn, d.form.asInstanceOf[DeclinableForm])
})

lazy val conjugatables = greekMorphObjects.filter( gmo => {
	gmo.form.typeName match {
		case "FiniteVerbForm" => true
		case "InfinitiveForm" => true
		case "ParticipleForm" => true
		case _ => false
	}
}).map( d => { 
	(d.urn, d.form.asInstanceOf[ConjugatableForm])
})


lazy val degreeables = greekMorphObjects.filter( gmo => {
	gmo.form.typeName match {
		case "AdjectiveForm" => true
		case "AdverbForm" => true
		case _ => false
	}
}).map( d => { 
	(d.urn, d.form.asInstanceOf[DegreeableForm])
})

lazy val nouns = greekMorphObjects.filter( _.form.typeName == "NounForm").map( d => { 
	(d.urn, d.form.asInstanceOf[NounForm])
})
lazy val pronouns = greekMorphObjects.filter( _.form.typeName == "PronounForm").map( d => { 
	(d.urn, d.form.asInstanceOf[PronounForm])
})
lazy val adjectives = greekMorphObjects.filter( _.form.typeName == "AdjectiveForm").map( d => { 
	(d.urn, d.form.asInstanceOf[AdjectiveForm])
})
lazy val articles = greekMorphObjects.filter( _.form.typeName == "ArticleForm").map( d => { 
	(d.urn, d.form.asInstanceOf[ArticleForm])
})


lazy val verbs = greekMorphObjects.filter( _.form.typeName == "FiniteVerbForm").map( d => { 
	(d.urn, d.form.asInstanceOf[FiniteVerbForm])
})
lazy val infinitives = greekMorphObjects.filter( _.form.typeName == "InfinitiveForm").map( d => { 
	(d.urn, d.form.asInstanceOf[InfinitiveForm])
})
lazy val participles = greekMorphObjects.filter( _.form.typeName == "ParticipleForm").map( d => { 
	(d.urn, d.form.asInstanceOf[ParticipleForm])
})

// Look up entries for words
case class MorphPairing(text:CtsUrn, morphology:Cite2Urn, lex:Option[Vector[Cite2Urn]] = None)

def morphPairs(corp:Corpus, morphObjects:Vector[CitableMorphology], lang:MorphLanguage):Vector[MorphPairing] = {
	corp.nodes.map( n => {
		lang match {
			case Greek => {
				val lgs:LiteraryGreekString = LiteraryGreekString(n.text)
				val morphs:Vector[CitableMorphology] = morphObjects.filter( mo => {
					LiteraryGreekString(mo.form.surfaceForm) == lgs
				})
				morphs.map( m => {
					MorphPairing(n.urn, m.urn, None)
				})
			}
			case _ => {
				val morphs:Vector[CitableMorphology] = morphObjects.filter( mo => {
					mo.form.surfaceForm == n.text
				})
				morphs.map( m => {
					MorphPairing(n.urn, m.urn, None)
				})
			}
		}
	}).flatten
}

def corpusForLang(tr:TextRepository, lang:String):Corpus = {
	val catEntries:Vector[CatalogEntry] = tr.catalog.texts.filter(_.lang == lang)
	val urns:Vector[CtsUrn] = catEntries.map(_.urn)
	tr.corpus ~~ urns
}

// Morph our Greek texts

lazy val analysisGreek:Vector[MorphPairing] = {
	val corp:Corpus = corpusForLang(textLib.textRepository.get, "grc")
	morphPairs(corp, greekMorphObjects, Greek)
}

lazy val analysisLatin:Vector[MorphPairing] = {
	val corp:Corpus = corpusForLang(textLib.textRepository.get, "lat")
	morphPairs(corp, latinMorphObjects, Latin)
}

