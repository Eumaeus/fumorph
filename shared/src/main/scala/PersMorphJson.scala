package edu.furman.classics.fumorph
import scala.scalajs.js
import scala.scalajs.js.annotation._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.dse._
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
		partOfSpeech:Option[String]
)


/** A Class for interacting with the Perseus Morphology Service
*
* @constructor create a new [[PerseusMorphService]] service
*/
@JSExportAll case class PerseusMorphService(str:String, lang:MorphLanguage) {
		val url:String = s"http://services.perseids.org/bsp/morphologyservice/analysis/word?lang=${lang.abbr}&engine=morpheus${lang.abbr}&mode=xml&word=${str}"
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
			val partOfSpeech:Option[String] = cursor.downField("rest").downField("entry").downField("dict").downField("pofs").downField("$").as[String].toOption
			PersEntry(surfaceForm, lang, headword, partOfSpeech)
		}




}