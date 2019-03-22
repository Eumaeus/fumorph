package edu.furman.classics.fumorph

import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import cats.syntax.either._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.scalatest.FlatSpec

class AnalysisTypeSpec extends FlatSpec {

	"An AnalysisType" should "report long, abbreviated, and short forms" in {
		val nom:GrammaticalCase = Nominative
		assert(nom.long == "nominative")
		assert(nom.abbr == "nom.")
		assert(nom.short == "n")
		val gen:GrammaticalCase = Genitive
		assert(gen.long == "genitive")
		assert(gen.abbr == "gen.")
		assert(gen.short == "g")
		val dat:GrammaticalCase = Dative
		assert(dat.long == "dative")
		assert(dat.abbr == "dat.")
		assert(dat.short == "d")
		val acc:GrammaticalCase = Accusative
		assert(acc.long == "accusative")
		assert(acc.abbr == "acc.")
		assert(acc.short == "a")
		val voc:GrammaticalCase = Vocative
		assert(voc.long == "vocative")
		assert(voc.abbr == "voc.")
		assert(voc.short == "v")
		val loc:GrammaticalCase = Locative
		assert(loc.long == "locative")
		assert(loc.abbr == "loc.")
		assert(loc.short == "l")
		val abl:GrammaticalCase = Ablative
		assert(abl.long == "ablative")
		assert(abl.abbr == "abl.")
		assert(abl.short == "b")
	}

	it should "include Gender" in {
		val masc:Gender = Masculine
		assert (masc.long == "masculine")
		assert (masc.abbr == "masc.")
		assert (masc.short == "m")
		val fem:Gender = Feminine
		assert (fem.long == "feminine")
		assert (fem.abbr == "fem.")
		assert (fem.short == "f")
		val neut:Gender = Neuter
		assert (neut.long == "neuter")
		assert (neut.abbr == "neut.")
		assert (neut.short == "n")
	}

	it should "include Number" in {
		val masc:GrammaticalNumber = Singular
		assert (masc.long == "singular")
		assert (masc.abbr == "sing.")
		assert (masc.short == "s")
		val fem:GrammaticalNumber = Dual
		assert (fem.long == "dual")
		assert (fem.abbr == "dl.")
		assert (fem.short == "d")
		val neut:GrammaticalNumber = Plural
		assert (neut.long == "plural")
		assert (neut.abbr == "pl.")
		assert (neut.short == "p")
	}

	it should "include Degree" in {
		val pos:Degree = Positive
		assert (pos.long == "positive")
		assert (pos.abbr == "pos.")
		assert (pos.short == "p")
		val comp:Degree = Comparative
		assert (comp.long == "comparative")
		assert (comp.abbr == "comp.")
		assert (comp.short == "c")
		val sup:Degree = Superlative
		assert (sup.long == "superlative")
		assert (sup.abbr == "sup.")
		assert (sup.short == "s")
	}
	
	it should "include Person" in {
		val first:Person = First 
		assert (first.long == "1st person")
		assert (first.abbr == "1st")
		assert (first.short == "1")
		val second:Person = Second 
		assert (second.long == "2nd person")
		assert (second.abbr == "2nd")
		assert (second.short == "2")
		val third:Person = Third 
		assert (third.long == "3rd person")
		assert (third.abbr == "3rd")
		assert (third.short == "3")
	}

	it should "include Tense" in pending
	it should "include Mood" in pending
	it should "include Voice" in pending
	it should "include IndeclinablePoS" in pending

}
