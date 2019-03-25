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
)


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