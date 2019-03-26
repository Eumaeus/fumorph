package edu.furman.classics.fumorph

import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import cats.syntax.either._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.scalatest.FlatSpec

class FuMorphObjectSpec extends FlatSpec {

	"The FuMorph Object" should "say have a language" in {
		val fmo:FuMorph = new FuMorph(Greek)
		assert ( fmo.lang.abbr == "grc" )
	}

}
