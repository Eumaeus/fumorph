package edu.furman.classics.fumorph
import scala.scalajs.js
import scala.scalajs.js.annotation._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.dse._
import edu.holycross.shot.greek._
import edu.holycross.shot.citerelation._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import io.circe.parser.decode
import cats.syntax.either._

/** A Class for holding entries from a Perseus Morphology reply
*
* @constructor create a new [[PerseusMorphService]] service
*/
@JSExportAll case class PersEntry(
	surfaceForm:String, 
	lang:MorphLanguage,
	lexLemma:Option[String],
	inflections:Vector[InflectionRec]
)

@JSExportAll case class InflectionRec(
	lang:Option[String],

	stem:Option[String],
	stemtype:Option[String],
	suff:Option[String],

	partOfSpeech:Option[String],
    dial:Option[String],

    grammaticalNum:Option[String],
    pers:Option[String],
    tense:Option[String],
    voice:Option[String],
	mood:Option[String],
    comp:Option[String],

    grammaticalCase:Option[String],
    gend:Option[String],
	morph:Option[String],
    derivtype:Option[String],
    decl:Option[String]
) {

	override def toString = {
		val sv:Vector[String] = {
			Vector(
				(if (lang != None) s"lang: ${lang.get}" else ""),
				(if (stem != None) s"stem: ${stem.get}" else ""),
				(if (stemtype != None) s"stemtype: ${stemtype.get}" else ""),
				(if (suff != None) s"suff: ${suff.get}" else ""),
				(if (partOfSpeech != None) s"partOfSpeech: ${partOfSpeech.get}" else ""),
				(if (dial != None) s"dial: ${dial.get}" else ""),
				(if (grammaticalNum != None) s"grammaticalNum: ${grammaticalNum.get}" else ""),
				(if (pers != None) s"pers: ${pers.get}" else ""),
				(if (tense != None) s"tense: ${tense.get}" else ""),
				(if (voice != None) s"voice: ${voice.get}" else ""),
				(if (mood != None) s"mood: ${mood.get}" else ""),
				(if (comp != None) s"comp: ${comp.get}" else ""),
				(if (grammaticalCase != None) s"grammaticalCase: ${grammaticalCase.get}" else ""),
				(if (gend != None) s"gend: ${gend.get}" else ""),
				(if (morph != None) s"morph: ${morph.get}" else ""),
				(if (derivtype != None) s"derivtype: ${derivtype.get}" else ""),
				(if (decl != None) s"decl: ${decl.get}" else "")
			).filter(_.size > 0)
		}
		sv.mkString("; ") + (".")
	}
}


/** A Class for interacting with the Perseus Morphology Service
*
* @constructor create a new [[PerseusMorphService]] service
*/
@JSExportAll case class PerseusMorphService(str:String, lang:MorphLanguage) {
		val url:String = s"http://services.perseids.org/bsp/morphologyservice/analysis/word?lang=${lang.abbr}&engine=morpheus${lang.abbr}&mode=xml&word=${str}"
}

/** A Class for interacting with the Perseus Morphology Service
*
* @constructor create a new [[PerseusMorphService]] service
*/
@JSExportAll case object PerseusParser {

	def getMorphJson(str:String, lang:MorphLanguage):String = {
		try {
			if ( (lang == Greek) | (lang == Latin)) {	

				val gStr:String = {
					if (lang == Greek) {
						val elided:Boolean = {
							str.last.toString.matches("['ʼ‘’’]")
						}
						val gs:LiteraryGreekString = LiteraryGreekString(str)
						if (elided) {
							val emended:String = gs.ascii.replaceAll("#","") + "%27"
							emended
						} else {
							gs.ascii.replaceAll("#","").replaceAll("\\\\","/")
						}
					} else {
						str
					}
				}

				val raw:String = scala.io.Source.fromURL(PerseusMorphService(gStr, lang).url).mkString
				raw
			} else { "" }
		} catch {
			case e:Exception => {
				""
			}
		}
	}

	def makeInfo(inf:InflectionRec):String = {
		val sv:Vector[String] = {
			Vector(
				(if (inf.stem != None) s"stem: ${inf.stem.get}" else ""),
				(if (inf.stemtype != None) s"stemtype: ${inf.stemtype.get}" else ""),
				(if (inf.suff != None) s"suff: ${inf.suff.get}" else ""),
				(if (inf.dial != None) s"dial: ${inf.dial.get}" else ""),
				(if (inf.morph != None) s"morph: ${inf.morph.get}" else ""),
				(if (inf.derivtype != None) s"derivtype: ${inf.derivtype.get}" else ""),
				(if (inf.decl != None) s"decl: ${inf.decl.get}" else "")
			).filter(_.size > 0)
		}
		sv.mkString("; ") + (".")
	}

	def analyzeText(c:Corpus, lang:MorphLanguage):Vector[PersEntry] = {
		val optionVec:Vector[Option[Vector[PersEntry]]] = {
			val splitters:String = """[\[\])(·⸁.,·;;   "?·!–—⸂⸃]"""
			c.nodes.filter(_.text.replaceAll(splitters,"").size > 0).map( n => {
				val u:CtsUrn = n.urn
				val s:String = n.text.replaceAll(splitters,"")
				val str:String = {
					if (lang == Greek) {
						val elided:Boolean = {
							s.last.toString.matches("['ʼ‘’’]")
						}
						val gs:LiteraryGreekString = LiteraryGreekString(s)
						if (elided) {
							val emended:String = gs.ascii.replaceAll("#","") + "%27"
							emended
						} else {
							gs.ascii.replaceAll("#","").replaceAll("\\\\","/")
						}
					} else {
						s
					}
				}
				println(s""" … (${lang.abbr}) "${str}" >> "${str}" """)
				val morph:String = getMorphJson(str, lang)
				if (morph.size < 10) {
					None
				} else {
					val pmj:PerseusMorphJson = PerseusMorphJson(morph, lang, str)
					pmj.entries
				}
			})
		}
		optionVec.filter(_ != None).map(_.get).flatten
	}

	def getGender(gs:String):Option[Gender] = {
		gs match {
			case "masculine" => Some(Masculine)
			case "feminine" => Some(Feminine)
			case "neuter" => Some(Neuter)
			case _ => None
		}
	}

	def getCase(cs:String):Option[GrammaticalCase] = {
		cs match {
			case "nominative" => Some(Nominative)
			case "genitive" => Some(Genitive)
			case "dative" => Some(Dative)
			case "accusative" => Some(Accusative)
			case "vocative" => Some(Vocative)
			case "locative" => Some(Locative)
			case "ablative" => Some(Ablative)
			case _ => None
		}
	}

	def getNumber(ns:String):Option[GrammaticalNumber] = {
		ns match {
			case "singular" => Some(Singular)
			case "dual" => Some(Dual)
			case "plural" => Some(Plural)
			case _ => None
		}
	}

	def getDegree(ns:String):Degree = {
		ns match {
			case "positive" => Positive
			case "comparative" => Comparative
			case "superlative" => Superlative
			case _ => Positive
		}
	}

	def getPerson(ns:String):Option[Person] = {
		ns match {
			case "1st" => Some(First)
			case "2nd" => Some(Second)
			case "3rd" => Some(Third)
			case _ => None
		}
	}

	def getTense(ns:String):Option[Tense] = {
		ns match {
			case "present" => Some(Present)
			case "imperfect" => Some(Imperfect)
			case "future" => Some(Future)
			case "aorist" => Some(Aorist)
			case "perfect" => Some(Perfect)
			case "pluperfect" => Some(Pluperfect)
			case "future perfect" => Some(FuturePerfect)
			case _ => None
		}
	}

	def getMood(ns:String):Option[Mood] = {
		ns match {
			case "indicative" => Some(Indicative)
			case "imperative" => Some(Imperative)
			case "subjunctive" => Some(Subjunctive)
			case "optative" => Some(Optative)
			case _ => None
		}
	}

	def getVoice(ns:String):Option[Voice] = {
		ns match {
			case "active" => Some(Active)
			case "middle" => Some(Middle)
			case "passive" => Some(Passive)
			case "mediopassive" => Some(MedioPassive)
			case "deponent" => Some(Deponent)
			case _ => None
		}
	}

	def toForms(pe:PersEntry):Vector[Form] = {
		if (pe.inflections.size < 1) {
			throw new MorphJsonException(s"""Exception: "${pe.surfaceForm}" has no inflections.""")
		}
		val fv:Vector[Form] = pe.inflections.map( inf => {
			val info:String = makeInfo(inf)
			inf.partOfSpeech match {
				case Some(pos) => {
					pos match {
						case "noun" => {
							val isSupine:Boolean = (inf.mood == Some("supine"))
							if (isSupine) {
								val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
								val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse("singular"))
								if ( (grammaticalCase != None) & (grammaticalNumber != None) ) {
									new SupineForm(
										pe.lang,
										pe.surfaceForm,
										pe.lexLemma,
										Neuter,
										grammaticalCase.get,
										grammaticalNumber.get,
										info	
									)
								} else {
									new InvalidForm(
										pe.lang,
										pe.surfaceForm,
										pe.lexLemma,
										inf.toString	
									)
								}
							} else {
								val gender:Option[Gender] = getGender(inf.gend.getOrElse(""))
								val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
								val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
								if ( (gender != None) & (grammaticalCase != None) & (grammaticalNumber != None) ) {
									new NounForm(
										pe.lang, 
										pe.surfaceForm, 
										pe.lexLemma, 
										gender.get, 
										grammaticalCase.get, 
										grammaticalNumber.get, 
										makeInfo(inf)
									)
								} else {
									new InvalidForm(
										pe.lang,
										pe.surfaceForm,
										pe.lexLemma,
										inf.toString	
									)
								}
							}
						}
						case "article" => {
							val gender:Option[Gender] = getGender(inf.gend.getOrElse(""))
							val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
							val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
							if ( (gender != None) & (grammaticalCase != None) & (grammaticalNumber != None) ) {
								new ArticleForm(
									pe.lang, 
									pe.surfaceForm, 
									pe.lexLemma, 
									gender.get, 
									grammaticalCase.get, 
									grammaticalNumber.get, 
									makeInfo(inf)
								)
							} else {
								new InvalidForm(
									pe.lang,
									pe.surfaceForm,
									pe.lexLemma,
									inf.toString	
								)
							}
						}
						case "pronoun" => {
							val gender:Option[Gender] = getGender(inf.gend.getOrElse(""))
							val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
							val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
							if ( (gender != None) & (grammaticalCase != None) & (grammaticalNumber != None) ) {
								new PronounForm(
									pe.lang, 
									pe.surfaceForm, 
									pe.lexLemma, 
									gender.get, 
									grammaticalCase.get, 
									grammaticalNumber.get, 
									makeInfo(inf)
								)
							} else {
								new InvalidForm(
									pe.lang,
									pe.surfaceForm,
									pe.lexLemma,
									inf.toString	
								)
							}
						}
						case "adjective" => {
							val testGender = (inf.gend == Some("adverbial"))
							if (testGender) {
								val degree:Degree = getDegree(inf.comp.getOrElse("positive"))
								new AdverbForm(
									pe.lang, 
									pe.surfaceForm, 
									pe.lexLemma,
									degree,
									info
								)
							} else {
								val gender:Option[Gender] = getGender(inf.gend.getOrElse(""))
								val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
								val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
								val degree:Degree = getDegree(inf.comp.getOrElse("positive"))
								if ( (gender != None) & (grammaticalCase != None) & (grammaticalNumber != None) ) {
									if (inf.stemtype == Some("verb_adj2")) {
										new VerbalAdjectiveForm(
											pe.lang, 
											pe.surfaceForm, 
											pe.lexLemma, 
											gender.get, 
											grammaticalCase.get, 
											grammaticalNumber.get, 
											info
										)
									} else {
										new AdjectiveForm(
											pe.lang, 
											pe.surfaceForm, 
											pe.lexLemma, 
											gender.get, 
											grammaticalCase.get, 
											grammaticalNumber.get, 
											degree,
											info
										)
									}
								} else {
									new InvalidForm(
										pe.lang,
										pe.surfaceForm,
										pe.lexLemma,
										inf.toString	
									)
								}
							}
						}
						case "verb" => {
							inf.mood match {
								case Some("infinitive") => {
									val tense:Option[Tense] = getTense(inf.tense.getOrElse(""))
									val voice:Option[Voice] = getVoice(inf.voice.getOrElse(""))
									if ( (tense != None) & (voice != None) ) {
										new InfinitiveForm(
											pe.lang,
											pe.surfaceForm,
											pe.lexLemma,
											tense.get,
											voice.get,
											info	
										)
									} else {
										new InvalidForm(
											pe.lang,
											pe.surfaceForm,
											pe.lexLemma,
											inf.toString	
										)
									}
								}
								case Some("gerundive") => {
									val gender:Option[Gender] = getGender(inf.gend.getOrElse(""))
									val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
									val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
									if ( (gender != None) & (grammaticalCase != None) & (grammaticalNumber != None) ) {
											new GerundiveForm(
											pe.lang, 
											pe.surfaceForm, 
											pe.lexLemma, 
											gender.get, 
											grammaticalCase.get, 
											grammaticalNumber.get, 
											makeInfo(inf)
										)
									} else {
										new InvalidForm(
											pe.lang,
											pe.surfaceForm,
											pe.lexLemma,
											inf.toString	
										)
									}
								}
								case _ => {
									val tense:Option[Tense] = getTense(inf.tense.getOrElse(""))
									val voice:Option[Voice] = getVoice(inf.voice.getOrElse(""))
									val person:Option[Person] = getPerson(inf.pers.getOrElse(""))
									val mood:Option[Mood] = getMood(inf.mood.getOrElse(""))
									val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
									if ( (tense != None) & (voice != None) & (person != None) & (grammaticalNumber != None) & ( mood != None) ) {
										new FiniteVerbForm(
											pe.lang,
											pe.surfaceForm,
											pe.lexLemma,
											person.get,
											grammaticalNumber.get,
											tense.get,
											mood.get,
											voice.get,
											info
										)
									} else {
										new InvalidForm(
											pe.lang,
											pe.surfaceForm,
											pe.lexLemma,
											inf.toString	
										)
									}
								}
							}
						}
						case "verb participle" => {
							val tense:Option[Tense] = getTense(inf.tense.getOrElse(""))
							val voice:Option[Voice] = getVoice(inf.voice.getOrElse(""))
							val gender:Option[Gender] = getGender(inf.gend.getOrElse(""))
							val grammaticalCase:Option[GrammaticalCase] = getCase(inf.grammaticalCase.getOrElse(""))
							val grammaticalNumber:Option[GrammaticalNumber] = getNumber(inf.grammaticalNum.getOrElse(""))
							if ( (gender != None) & (grammaticalCase != None) & (grammaticalNumber != None) & (tense != None) & (voice != None) ) {
								new ParticipleForm(
									pe.lang,
									pe.surfaceForm,
									pe.lexLemma,
									tense.get,
									voice.get,
									gender.get,
									grammaticalCase.get,
									grammaticalNumber.get,
									info	
								)
							} else {
								new InvalidForm(
									pe.lang,
									pe.surfaceForm,
									pe.lexLemma,
									inf.toString	
								)
							}
						}
						case "adverb" => {
							val degree:Degree = getDegree(inf.comp.getOrElse("positive"))
							new AdverbForm(
								pe.lang, 
								pe.surfaceForm, 
								pe.lexLemma,
								degree,
								info
							)
						}
						case "particle" => {
							new IndeclinableForm(
								pe.lang,
								pe.surfaceForm,
								pe.lexLemma,
								Particle,
								inf.toString	
							)
						}
						case "conjunction" => {
							new IndeclinableForm(
								pe.lang,
								pe.surfaceForm,
								pe.lexLemma,
								Conjunction,
								inf.toString	
							)
						}
						case "preposition" => {
							new IndeclinableForm(
								pe.lang,
								pe.surfaceForm,
								pe.lexLemma,
								Preposition,
								inf.toString	
							)
						}
						case _ => {
							new InvalidForm(
								pe.lang,
								pe.surfaceForm,
								pe.lexLemma,
								inf.toString	
							)
						}
					}
				}	
				case None => {
					new InvalidForm(
						pe.lang,
						pe.surfaceForm,
						pe.lexLemma,
						inf.toString	
					)
				}
			}	
		})
		fv
	}

	def toForms(persEntries:Vector[PersEntry]):Vector[Form] = {
		persEntries.map(pe => toForms(pe)).flatten
	}
}

/** A Class for parsing JSON from the  Perseus Morphology Service
*
* @constructor create a new [[PerseusMorphJson]] class
*/
@JSExportAll case class PerseusMorphJson(json:String, lang:MorphLanguage, surfaceForm:String) {

		val doc:Json = parse(json).getOrElse(Json.Null)
		if (doc == Json.Null) {
			throw new MorphJsonException(s"""PerseusMorphJson Exception: Invalid JSON associated with "${surfaceForm}" [${lang.abbr}]: < ${json} >""")
		}

		val entries:Option[Vector[PersEntry]] = {
				val cursor:HCursor = doc.hcursor
				val entriesCursor:ACursor = cursor.downField("RDF").downField("Annotation").downField("Body")
				val entryList:Option[List[Json]] = entriesCursor.as[List[Json]].toOption
				entryList match {
					case Some(el) => {
						val entryVec:Vector[PersEntry] = el.toVector.map( entry => {
							getPersEntry(entry, lang, surfaceForm)
						})
						Some(entryVec)
					}
					case None => {
						val tryOne:Option[Json] = entriesCursor.as[Json].toOption
						tryOne match {
							case Some(e) => {
								val entry:PersEntry = getPersEntry(e, lang, surfaceForm)
								Some(Vector(entry))
							}
							case None => {
								val nel:Option[Vector[PersEntry]] = None
								nel
							}
						}
					}
				}
		}

		def getPersEntry(json:Json, lang:MorphLanguage, surfaceForm:String):PersEntry = {
			val cursor:HCursor = json.hcursor
			val headword:Option[String] = cursor.downField("rest").downField("entry").downField("dict").downField("hdwd").downField("$").as[String].toOption
			val inflections:Vector[InflectionRec] = {
				val iList:List[Json] = processInflJson(json)
				inflList(iList)
			}

			PersEntry(surfaceForm, lang, headword, inflections)
		}

		def inflList(jList: List[Json]):Vector[InflectionRec] = {
			jList.toVector.map( il => {
				val ic:HCursor = il.hcursor
				val is:Option[Iterable[String]] = ic.keys

				val lang = ic.downField("term").downField("lang").as[String].toOption
				val stem = ic.downField("term").downField("stem").downField("$").as[String].toOption
				val suff = ic.downField("term").downField("suff").downField("$").as[String].toOption
				val pofs = ic.downField("pofs").downField("$").as[String].toOption
				val mood = ic.downField("mood").downField("$").as[String].toOption
				val tense = ic.downField("tense").downField("$").as[String].toOption
				val pers = ic.downField("pers").downField("$").as[String].toOption
				val voice = ic.downField("voice").downField("$").as[String].toOption
				val num = ic.downField("num").downField("$").as[String].toOption
				val stemtype = ic.downField("stemtype").downField("$").as[String].toOption
				val dial = ic.downField("dial").downField("$").as[String].toOption
				val comp = ic.downField("comp").downField("$").as[String].toOption
				val grammaticalCase = ic.downField("case").downField("$").as[String].toOption
				val gend = ic.downField("gend").downField("$").as[String].toOption
				val morph = ic.downField("morph").downField("$").as[String].toOption
				val derivtype = ic.downField("derivtype").downField("$").as[String].toOption
				val decl = ic.downField("decl").downField("$").as[String].toOption

				new InflectionRec(
					lang = lang,
					stem = stem,
					stemtype = stemtype,
					suff = suff,

					partOfSpeech = pofs,
				    dial = dial,

				    grammaticalNum = num,
				    pers = pers,
				    tense = tense,
				    voice = voice,
					mood = mood,
				    comp = comp,

				    grammaticalCase = grammaticalCase,
				    gend = gend,
					morph = morph,
				    derivtype = derivtype,
				    decl = decl			
				)
			})
		}


		/**
		* Returns a List[Json] consisting of the contents of the "infl" element.
		**/
		def processInflJson(json:Json):List[Json] = {
			val cursor:HCursor = json.hcursor
			/* the `infl` thing in the JSON is either an object or a list */
			val inflListOpt:Option[List[Json]] = cursor.downField("rest").downField("entry").downField("infl").as[List[Json]].toOption

			val inflObjOpt:Option[Json] = cursor.downField("rest").downField("entry").downField("infl").as[Json].toOption

			/* If neither, throw an error */
			if ((inflListOpt == None) & (inflObjOpt == None)) {
				throw new MorphJsonException(s"""Found no `infl` data for "${surfaceForm} [${lang}].""")
			}

			/* Make a list, one way or another */
			val inflList:List[Json] = {
				inflListOpt match {
					case Some(il) => {
						il
					}
					case None => {
						List(inflObjOpt.getOrElse(Json.Null))
					}
				}
			}
			inflList
		}

/*
		def getInflEntry(json:Json, field:String):Option[String] = {
			val val:Option[String] = cursor.downField("rest").downField("entry").downField("dict").downField("pofs").downField("$").as[String].toOption
		}
		*/
}


