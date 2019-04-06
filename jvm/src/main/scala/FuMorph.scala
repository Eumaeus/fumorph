package edu.furman.classics.fumorph
import scala.scalajs.js
import scala.scalajs.js.annotation._
import edu.holycross.shot.cite._
import edu.holycross.shot.greek._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._


@JSExportAll
case class FuMorph(morphLib:Option[CiteLibrary], textLib:CiteLibrary, lang:MorphLanguage, lexIndex:Vector[String] = Vector[String]() ) {

	val morphCex:MorphCex = MorphCex(lang)

   private val punctuation:String = """[\[\])(·⸁.,·;;   "?·!–—⸂⸃]"""

	private val morphModel = Cite2Urn("urn:cite2:cite:datamodels.v1:fumorph")
	private val lgsModel = Cite2Urn("urn:cite2:cite:datamodels.v1:lgs")


	lazy val archivedIntVec:Vector[Int] = {
		Vector(1,2,3)
	}

	def archivedFormVec:Vector[Form] = {
		this.morphLib match {
			case Some(ml) => {
				val collRepOption:Option[CiteCollectionRepository] = ml.collectionRepository
				val morphCollections:Vector[Cite2Urn] = ml.collectionsForModel(morphModel)
				val surfaceFormProperties:Vector[Cite2Urn] = {
					val sfStr:String = "surfaceform"
					morphCollections.map( _.addProperty(sfStr))
				}
				val lemmaProperties:Vector[Cite2Urn] = {
					val lemStr:String = "lemma"
					morphCollections.map( _.addProperty(lemStr))
				}
				val lgsProperties:Vector[Cite2Urn] = ml.collectionsForModel(lgsModel)
				val ff:Vector[CitableMorphology] = morphCex.formsFromCiteObjects(ml)
				//println(s"\n\nff:\n\n${ff}\n\n")
				ff.map(_.form).distinct
			}
			case None => {
				println(s"morphLib: None : ${morphLib}")
				Vector[Form]()
			}
		}
	}
	
	

	val archivedForms:Vector[String] = {
		if (lang == Greek) {
			archivedFormVec.map( f => {
				LiteraryGreekString(f.surfaceForm).ucode
			}).distinct
		} else {
			archivedFormVec.map(_.surfaceForm).distinct
		}	
	}

	lazy val langCorpus:Option[Corpus] = {
		println(s"Building a Corpus for '${lang.abbr}'…")
		textLib.textRepository match {
			case Some(tr) => {
				val catEntries:Vector[CatalogEntry] = tr.catalog.texts.filter(_.lang == lang.abbr )
				val urns:Vector[CtsUrn] = catEntries.map(_.urn)
				val newCorp:Corpus = tr.corpus ~~ urns
				Some(newCorp)
			}
			case None => None
		}
	}

	lazy val unknownFormsCorpus:Corpus = {
		println(s"Building a corpus of undocumented tokens…")
		langCorpus match {
			case Some(corp) => {
				val nodes:Vector[CitableNode] = corp.nodes.filter( n => {
					if (lang == Greek) {
						val gs:String = normalizeGreek(n.text)	
						((archivedForms.contains(gs) == false) &
						(gs.size > 0))
					} else {
						val ls:String = normalizeLatin(n.text)
						((archivedForms.contains(ls) == false) &
						(ls.size > 0))
					}
				}).map( n => {
					val u = n.urn
					val t = {
						if (lang == Greek) {
							normalizeGreek(n.text)
						} else {
							normalizeLatin(n.text)
						}
					}
					CitableNode(u,t)
				})
				val texts:Vector[String] = nodes.map(_.text).distinct
				println(s"${texts.size} distinct new forms.")
				val newCorp:Corpus = {
					val us:String = "urn:cts:fuTest:g.w.v:"
					val newNodes = texts.zipWithIndex.map{ case(t,i) => {
						CitableNode( CtsUrn(s"${us}${i}"), t)
					}}
					Corpus(newNodes)
				}
				newCorp
			}
			case None => {
				Corpus(Vector[CitableNode]())
			}
		}
	}

	lazy val newPersForms:Vector[PersEntry] = {
		PerseusParser.analyzeText(unknownFormsCorpus, lang)
	}

	lazy val newFormVec:Vector[Form] = {
		PerseusParser.toForms(newPersForms)
	}

	lazy val updatedForms:Vector[Form] = {
		(archivedFormVec ++ newFormVec).distinct.sortBy(_.lemma)
	}

	lazy val backupCex:String = morphCex.cex(lang, archivedFormVec, "fumorph", "temp", "#")

	lazy val updateCex:String = morphCex.cex(lang, updatedForms, "fumorph", "temp", "#")

	lazy val forms:Vector[CitableMorphology] = {
		morphLib match {
			case Some(ml) => morphCex.formsFromCiteObjects(ml)
			case None => Vector[CitableMorphology]()
		}
	}


/* Normalize Strings for matching */

	def normalizeGreek(s:String):String = {
		//println(s"""normalizing "${s}".""")
		// remove punctuation
		val puncStr:String = s.replaceAll(punctuation,"")
		// fix elision
		val elided:Boolean = {
			if (puncStr.size < 1) {
				false
			} else {
				puncStr.last.toString.matches("['ʼ‘’’᾽]")
			}
		}
		val emendedStr:String = {
			if (elided) {
				val lgs = LiteraryGreekString(puncStr).ucode.dropRight(1).replaceAll("#","") + "'"
				//println(s"lgs = ${lgs} ")
				lgs
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
		//println(s"""normalized "${finalString}".""")
		finalString
	}

	def normalizeLatin(s:String):String = {
		s.replaceAll(punctuation,"")
	}

}

