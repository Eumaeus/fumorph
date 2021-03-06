package edu.furman.classics.fumorph
import edu.holycross.shot.cite._
import java.util.Calendar

object UrnGenerator {
	def get(nameSpace:String, collectionName:String, versionName:String = "temp", idx:Int = 0):Cite2Urn = {

		val r = scala.util.Random
		val nr1:String = r.nextInt(100).toString
		val nr2:String = r.nextInt(100).toString
		val mss:String = {
			val now = Calendar.getInstance()
			val millis = now.getTimeInMillis
			millis.toString
		}
		Cite2Urn(s"urn:cite2:${nameSpace}:${collectionName}.${versionName}:${mss}${nr1}${nr2}${idx}")
	}
}
