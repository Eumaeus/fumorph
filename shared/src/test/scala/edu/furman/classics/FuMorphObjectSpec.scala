package edu.furman.classics.fumorph

import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import cats.syntax.either._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.scalatest.FlatSpec

class MorphCexObjectSpec extends FlatSpec {

	"The MorphCex Object" should "say have a language" in {
		val fmo:MorphCex = new MorphCex(Greek)
		assert ( fmo.lang.abbr == "grc" )
	}

}
