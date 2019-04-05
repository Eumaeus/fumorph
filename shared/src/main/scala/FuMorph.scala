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
		println(s"got here")
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
				println(s"\n\nff:\n\n${ff}\n\n")
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



	/*
	lazy val lemmata:Vector[String] = {
		collRepOption match {
			case Some(cr) => {
				val props:Vector[CitePropertyValue] = lemmaProperties.map( sfp => {
					cr.data.data.filter(_.urn ~~ sfp)
				}).flatten
				lang match {
					case Greek => {
						val greekStrings:Vector[LiteraryGreekString] = {
							props.map(p => LiteraryGreekString(p.propertyValue.asInstanceOf[String]))	
						}
						greekStrings.distinct.map(_.ucode)
					}
					case _ => {
						props.map(p => p.propertyValue.asInstanceOf[String]).distinct
					}
				}
			}
			case None => { Vector[String]() }
		}
	}
	*/

	lazy val langCorpus:Option[Corpus] = {
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
				Corpus(nodes)
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

	lazy val updateCex:String = morphCex.cex(lang, updatedForms, "fumorph", "temp", "#")

	lazy val forms:Vector[CitableMorphology] = {
		morphLib match {
			case Some(ml) => morphCex.formsFromCiteObjects(ml)
			case None => Vector[CitableMorphology]()
		}
	}

	lazy val morphRelations:CiteRelationSet = {
		val relationUrn:Cite2Urn = Cite2Urn("urn:cite2:fumorph:verbs.v1:mayParse")
		val triples:Vector[CiteTriple] = langCorpus.getOrElse(Corpus(Vector[CitableNode]())).nodes.map( n => {
			val t:String = {
				if (lang == Greek) {
					normalizeGreek(n.text)
				} else {
					normalizeLatin(n.text)
				}
			}
			println(s"finding morphs on: ${t}")
			val formSet:Vector[Cite2Urn] = {
				forms.filter( f => {
					val formStr:String = {
						if (lang == Greek) normalizeGreek(f.form.surfaceForm)
						else normalizeLatin(f.form.surfaceForm)
					}
					formStr == t
				}).map(_.urn)
			}
			println(s"got: ${formSet.size} formSets.")
			formSet.map( fs => {
				CiteTriple(n.urn, relationUrn, fs)
			})
		}).flatten
		CiteRelationSet(triples.toSet)
	}		

	lazy val lexRelations:CiteRelationSet = {
		val relationUrn:Cite2Urn = Cite2Urn("urn:cite2:fumorph:verbs.v1:maybeLemma")
		val triples:Vector[CiteTriple] = langCorpus.getOrElse(Corpus(Vector[CitableNode]())).nodes.map( n => {
			val t:String = {
				if (lang == Greek) {
					val ng:String = normalizeGreek(n.text)
					if (ng.size == 0) { "" }
					else {
						val beta:String = LiteraryGreekString(ng).ascii
						beta.replaceAll("[')(/=+]","")
					}
				} else {
					normalizeLatin(n.text)
				}
			}
			println(s"finding lex for: ${t}")
			val lexSet:Vector[Cite2Urn] = {
				lexIndex.filter( ll => {
					( ll.split("#")(1) == t)
				}).map( ll => {
					val uStr:String = ll.split("#")(0)
					Cite2Urn(uStr)
				})
			}
			lexSet.map( fs => {
				CiteTriple(n.urn, relationUrn, fs)
			})
		}).flatten
		CiteRelationSet(triples.toSet)
	}


/* Normalize Strings for matching */

	def normalizeGreek(s:String):String = {
		// remove punctuation
		val puncStr:String = s.replaceAll(punctuation,"")
		// fix elision
		val elided:Boolean = {
			if (puncStr.size < 1) {
				false
			} else {
				puncStr.last.toString.matches("['ʼ‘’’]")
			}
		}
		val emendedStr:String = {
			if (elided) {
				LiteraryGreekString(puncStr).ucode.dropRight(1).replaceAll("#","") + "'"
			} else {
				puncStr
			}
		}
		// make unicode
		val acuteString:String = {
			if (emendedStr.size < 1) { "" }
			else { LiteraryGreekString(emendedStr).ascii.replaceAll("\\\\","/").replaceAll("\\+","") }
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
		finalString
	}

	def normalizeLatin(s:String):String = {
		s.replaceAll(punctuation,"")
	}

}

