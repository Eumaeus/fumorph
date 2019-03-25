package edu.furman.classics.fumorph

import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import cats.syntax.either._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.scalatest.FlatSpec

class FuMorphSpec extends FlatSpec {

	"The FuMorph library" should "allow creation of a FiniteVerbForm" in {
		val fvf:FiniteVerbForm = FiniteVerbForm(
			lang = Greek,
			surfaceForm = "λύομεν",
			lemma = Some("λύω"),
			person = First,
			grammaticalNumber = Plural,
			tense = Present,
			voice = Active,
			mood = Indicative
		)
		assert( fvf.toString.trim == "**λύομεν** (from λύω): finite verb, 1st pl., pres. act. indic.")
		assert( fvf.toPos == "v1ppai---")
	}

	it should "allow the creation of an IndeclinableForm" in {
		val indeclf:IndeclinableForm = IndeclinableForm(
			lang = Greek,
			surfaceForm = "δέ",
			lemma = Some("δέ"),
			pos = Particle
		)
		assert(indeclf.toString.trim == "**δέ**: particle")

	}

	it should "allow the creation of an NounForm" in {
		val nounF:NounForm = NounForm(
			lang = Greek,
			surfaceForm = "ὁδῶν",
			lemma = Some("ὅδος"),
			gender = Feminine,
			grammaticalCase = Genitive,
			grammaticalNumber = Plural
		)
		assert(nounF.toString.trim == "**ὁδῶν** (from ὅδος): noun, fem., gen., pl.")
		assert( nounF.toPos == "n-p---fg-")
	}

}
