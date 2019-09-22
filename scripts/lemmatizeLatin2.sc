
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

//:load lemmatizeGreek.sc
//analyze

val punctuation:String = """[-\[\]·⸁.,·:…;;   "?·!–—⸂⸃]"""


def showMe(v:Any):Unit = {
	v match {
		case _:Vector[Any] => println(s"""----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----""")
		case _:Iterable[Any] => println(s"""----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----""")
		case _ => println(s"-----\n${v}\n----")
	}
}

val corp = mta.textRepo.corpus

val forms:Vector[MorphPair] = mta.formsForCorpus

val lemmatizedCorpus:Corpus = {
	val corpNodes:Vector[CitableNode] = corp.nodes.map( n => {
		val formOption:Option[MorphPair] = forms.find(_.text == n.urn )
		val prelemma:String = formOption match {
			case Some(form) => {
				val foundLemma:String = form.morph.map( m => {
					m.defs.map(_.lemma)
				}).flatten.distinct.mkString(" ")
				if (foundLemma == "") n.text
				else foundLemma
			}
			case None => {
				s"""${n.text.trim.replaceAll(punctuation,"")}"""
			}
		}
		val lemma:String = {
			if (lang == Greek){
				val lgs = LiteraryGreekString(prelemma)
				lgs.ucode
			} else {
				prelemma
			}
		}
		val newUrn:CtsUrn = n.urn.dropExemplar.addExemplar("lemmatizedToken")

		CitableNode(newUrn, lemma.replaceAll(" ","_"))
	})
	Corpus(corpNodes)
}

val editionCorpus:Corpus = {
	val oldEditionLabel:String = {
		val u:CtsUrn = lemmatizedCorpus.nodes.head.urn
		u.version 
	}
	val newEditionLabel:String = s"${oldEditionLabel}_lemmatized"
	collapseNodes(lemmatizedCorpus)
}

val bothCorps:Corpus = lemmatizedCorpus ++ editionCorpus


val cexString:String = {
	bothCorps.nodes.map(n => s"${n.urn}#${n.text}").mkString("\n")
}


saveString(cexString,"/Users/cblackwell/Desktop/lemmatized_latin.txt")