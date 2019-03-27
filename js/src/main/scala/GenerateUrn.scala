package edu.furman.classics.fumorph
import edu.holycross.shot.cite._
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSExportAll
object UrnGenerator {
	def get(nameSpace:String, collectionName:String, versionName:String = "temp"):Cite2Urn = {
		val r = scala.util.Random
		val nr1:String = r.nextInt(100).toString
		val nr2:String = r.nextInt(100).toString
		val mss:String = {
			val d = new js.Date()
			val millis = s"${d.getHours()}${d.getMinutes()}${d.getSeconds()}${d.getMilliseconds()}"
			millis.toString
		}
		Cite2Urn(s"urn:cite2:${nameSpace}:${collectionName}.${versionName}:${mss}${nr1}${nr2}")
	}
}
