package edu.furman.classics.fumorph
import scala.io.Source
import java.io._
import edu.holycross.shot.cite._
import edu.holycross.shot.greek._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._

case class ShortDef(urn:Cite2Urn, lemma:String, presentationLemma:String, definition:String) {
	val html:String = {
		s"""
<div class="fumorph_shortDef" data-lexicon-urn="${urn}">
	<span class="fumorph_shortDef_lemma">${presentationLemma}</span>
	<span class="fumorph_shortDef_definition">${definition.take(400)}</span>
</div>	
		"""	
	}
}
case class MorphWithLex(morph:CitableMorphology, defs:Vector[ShortDef]) {
	val html:String = {
		val defString:String = defs.map(_.html).mkString("\n")
	   s"""
	   <div class="fumorph_morphWithLex">
	   	${morph.html}
	   	<div class="fumorph_defs">${defString}</div>
	   </div>
		"""
	}
}
case class MorphPair(text:CtsUrn, morph:Vector[MorphWithLex]) {
	val html:String = s"""
		<div class="fumorph_alignedMorphology" data-textUrn="${text}">
			${morph.map(_.html).mkString("\n")}
		</div>
	"""
}

case class LexIndex(entries:Map[String,Vector[ShortDef]])
object LexIndex {
	def apply(fs:String) = {
		val lines:Vector[String] = Source.fromFile(fs).getLines.toVector	
		val splitted:Vector[Vector[String]] = lines.map(_.split("#").toVector).filter(_.size == 4)
		val mapped:Map[String,Vector[Vector[String]]] = splitted.groupBy(_(1))
		val mappedToEntries = {
			mapped.toVector.map( m => {
				val mKey:String = m._1
				val mVal:Vector[ShortDef] = m._2.map( mm => {
					ShortDef(Cite2Urn(mm(0)), mm(1), mm(2), mm(3))
				})
				(mKey, mVal)
			}).toMap
		}
		new LexIndex(mappedToEntries)
	}
}


case class MorphTextAligner(lang:MorphLanguage, textLib:CiteLibrary, morphLib:FuMorph, lexLib:CiteLibrary, lexIdx:LexIndex, lexVersionShort:String = "short", lexVersionFull:String = "full" ) {

	if (textLib.textRepository == None) throw new Exception("The 'textLib' must include a TextRepository.")
	if (lexLib.collectionRepository == None) throw new Exception("The 'lexLib' must include a CiteCollectionRepository.")
	if (morphLib.langCorpus == None)  throw new Exception(s"The supplied textRepository does not have texts in '${lang.abbr}.")
	// if we are here, we are good.

	val textRepo = textLib.textRepository.get
	val lexRepo = lexLib.collectionRepository.get

	val corp:Corpus = morphLib.langCorpus.get

	/* Greek to LexMatch */
	def greekForLexIndex(s:String):String = {
		val diacriticals:String = "[-*#]"
		val numbersStripped:String = s.replaceAll("[0-9]","")
		val lgs:LiteraryGreekString = LiteraryGreekString(numbersStripped)
		lgs.ascii.replaceAll(diacriticals,"").toLowerCase
	}

	/* Normalize Strings for matching */
	val punctuation:String = """[\[\]·⸁.,·;;   "?·!–—⸂⸃]"""
	def normalizeGreek(s:String):String = {
		//println(s"""normalizing "${s}".""")
		// remove punctuation and numbers
		val puncStr:String = s.replaceAll(punctuation,"").replaceAll("[0-9]","")
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

	val uniqueWords:Vector[(String, Vector[CitableNode])] = {
			println(s"${corp.nodes.size} total words…")
			val uw = lang match {
				case Greek => {
					corp.nodes.groupBy( n => normalizeGreek(n.text)).toVector	
				}
				case _ => {
					corp.nodes.groupBy( n => normalizeLatin(n.text)).toVector	
				}
			}
			println(s"…${uw.size} unique words.")
			uw
	}

	def findMatchingForms(uw:String):Vector[CitableMorphology] = {
		try {
			val lookUpString:String = uw
			val foundForms:Vector[CitableMorphology] = morphLib.surfaceFormMap(lookUpString)
			//println(s"Found ${foundForms.size} forms.")
			foundForms
		} catch {
			case e:Exception => Vector[CitableMorphology]()
		}
	}

	def formsForCorpus:Vector[MorphPair] = {
		// 0. uniqueWords : (surfaceForm, Vector[CtsUrn])
		val totalWords:Int = corp.nodes.size
		val distinctWords:Int = uniqueWords.size

		println(s"Doing `formsForCorpus` on ${distinctWords} distinct words out of ${totalWords} words.")

		//1. make a map(surfaceForm, citableMorphology)
		println(s"Finding morphology for unique forms…")
		val citableMorphMap:Vector[(String, Vector[CitableMorphology])] = {
			uniqueWords.map( uw => {
				val cmv:Vector[CitableMorphology] = findMatchingForms(uw._1)
				(uw._1, cmv)		
			})
		}
		// 2. make a map(lemmaString, shortDef)
		// 2a. get all unique lemmas
		println(s"Getting unique lemmata for parsed forms…")
		val allUniqueLemmas:Vector[String] = {
			val unflattened = citableMorphMap.map(_._2.map(_.form.lemma.getOrElse("no_lemma")))
			val flattened = unflattened.flatten
			val distinctLemmata = flattened.distinct
			println(s"got ${distinctLemmata.size} lemmata out of ${flattened.size}.")
			distinctLemmata
		}
		println(s"""${allUniqueLemmas.mkString("\n")}""")
		println(s"Done: ${allUniqueLemmas.size} lemmata")
		// 2b. get urns and short defs for them
		println(s"Getting short definitions…")
		val defsForForm:Vector[ShortDef] = {
			allUniqueLemmas.map( ul => {
					try {
						lang match {
							case Greek => {
								val s:String = greekForLexIndex(ul)
								println(s"Looking up lemma: ${s}")
								lexIdx.entries(s).map(e => {
									println(s"found in lexIdx: ${e}")
									val urn:Cite2Urn = e.urn
									val lem:String = e.lemma
									val presLem:String = e.presentationLemma
									val definition:String = e.definition
									ShortDef(urn, lem, presLem, definition)
								})
							}
							case _ => {
								val s:String = ul
								lexIdx.entries(s).map(e => {
									val urn:Cite2Urn = e.urn
									val lem:String = e.lemma
									val presLem:String = e.presentationLemma
									val definition:String = e.definition
									ShortDef(urn, lem, presLem, definition)
								})
							}
						}
					} catch {
						case e:Exception => Vector[ShortDef]()
					}
				}).flatten
		}
		println(s"Done: ${defsForForm.size} short definitions.")
		// 2d. map it up to a Map[surfaceForm, MorphWithLex]
		//				We have…
		//							citableMorphMap:Vector[(surfaceForm, Vector[CitableMorphology])]
		//							defsForForm:Vector[ShortDef]
		println(s"Mapping data to tokens in the text…")
		val unMappedMorph:Vector[CitableMorphology] = {
			citableMorphMap.map(_._2).flatten
		}
		val mappedDefs = defsForForm.groupBy(_.lemma)
		val morphsWithLexes = unMappedMorph.map( umm => {
			val lexes:Vector[ShortDef] = {
				try {
					lang match {
						case Greek => {
							umm.form.lemma match {
								case Some(l) => {
									val s:String = greekForLexIndex(l)
									mappedDefs(s)
								}
								case None => Vector[ShortDef]()
							}
						}
						case _ => {
							mappedDefs(umm.form.lemma.getOrElse("no_lemma"))
						}
					}
				} catch {
					case e:Exception => Vector[ShortDef]()
				}
			}
			MorphWithLex(umm, lexes)
		})
		println(s"\tMapping unique tokens in text to their citations…")
		val surfFormMap:Map[String, Vector[MorphWithLex]] = morphsWithLexes.groupBy(_.morph.form.surfaceForm)
		println(s"\tDone…")
		// 3. map these to our citable node urns
		println(s"\tMapping analyses to citable tokens…")
		val morphPairs:Vector[MorphPair] = {
			corp.nodes.map( n => {
				try {
					val s:String = {
						lang match {
							case Greek => normalizeGreek(n.text)
							case _ => normalizeLatin(n.text)
						}
					}
					val mwl:Vector[MorphWithLex] = surfFormMap(s)
					MorphPair(n.urn, mwl)
				} catch {
					case e:Exception => {
						MorphPair(n.urn, Vector[MorphWithLex]())
					}
				}

			})
		}
		println(s"\tDone. ${morphPairs.size} maps of token-analysis.")

		morphPairs
	}

	



}

