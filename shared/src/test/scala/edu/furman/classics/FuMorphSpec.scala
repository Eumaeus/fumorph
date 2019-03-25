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

	it should "allow the creation of an AdjectiveForm" in {
		val adjF:AdjectiveForm = AdjectiveForm(
			lang = Greek,
			surfaceForm = "καλόν",
			lemma = Some("καλός"),
			gender = Neuter,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular
		)
		assert(adjF.toString.trim == "**καλόν** (from καλός): adjective, neut., acc., sing. (pos. degree)")
		assert( adjF.toPos == "a-s---nap")
	}

	it should "allow the creation of an AdjectiveForm with a degree" in {
		val adjF:AdjectiveForm = AdjectiveForm(
			lang = Greek,
			surfaceForm = "κάλλιστον",
			lemma = Some("καλός"),
			gender = Neuter,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular,
			degree = Superlative
		)
		assert(adjF.toString.trim == "**κάλλιστον** (from καλός): adjective, neut., acc., sing. (sup. degree)")
		assert( adjF.toPos == "a-s---nas")
	}

	it should "allow the creation of a PronounForm" in {
		val pronF:PronounForm = PronounForm(
			lang = Greek,
			surfaceForm = "ταῦτα",
			lemma = Some("οὗτος"),
			gender = Neuter,
			grammaticalCase = Accusative,
			grammaticalNumber = Plural
		)
		assert(pronF.toString.trim == "**ταῦτα** (from οὗτος): pronoun, neut., acc., pl.")
		assert( pronF.toPos == "p-p---na-")
	}

	it should "allow the creation of an ArticleForm" in {
		val pronF:ArticleForm = ArticleForm(
			lang = Greek,
			surfaceForm = "αἱ",
			lemma = Some("ὁ"),
			gender = Feminine,
			grammaticalCase = Nominative,
			grammaticalNumber = Plural
		)
		assert(pronF.toString.trim == "**αἱ** (from ὁ): article, fem., nom., pl.")
		assert( pronF.toPos == "l-p---fn-")
	}

	it should "allow the creation of an AdverbForm" in {
		val advF:AdverbForm = AdverbForm(
			lang = Greek,
			surfaceForm = "καλῶς",
			lemma = Some("καλῶς"),
		)
		assert(advF.toString.trim == "**καλῶς** (from καλῶς): adverb (pos. degree)")
		assert( advF.toPos == "d-------p")
	}

	it should "allow the creation of an InfinitiveForm" in {
		val infF:InfinitiveForm = InfinitiveForm(
			lang = Latin,
			surfaceForm = "amare",
			lemma = Some("amo"),
			tense = Present,
			voice = Active
		)
		assert(infF.toString.trim == "**amare** (from amo): pres. act. infinitive")
		assert( infF.toPos == "v--pan---")
	}

	it should "allow the creation of a ParticipleForm" in {
		val partF:ParticipleForm = ParticipleForm(
			lang = Greek,
			surfaceForm = "οἰκουμένην",
			lemma = Some("οἰκέω"),
			tense = Present,
			voice = Passive,
			gender = Feminine,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular
		)
		assert(partF.toString.trim == "**οἰκουμένην** (from οἰκέω): pres. pass. participle, fem. acc. sing.")
		assert( partF.toPos == "v-spppfa-")
	}

	it should "allow the creation of a VerbalAdjectiveForm" in {
		val vaF:VerbalAdjectiveForm = VerbalAdjectiveForm(
			lang = Greek,
			surfaceForm = "διαβατέος",
			lemma = Some("διαβατέος"),
			gender = Masculine,
			grammaticalCase = Nominative,
			grammaticalNumber = Singular
		)
		assert(vaF.toString.trim == "**διαβατέος** (from διαβατέος): verbal adjective, masc., nom., sing.")
		assert( vaF.toPos == "a-s---mn-")
	}

	it should "allow the creation of a GerundForm" in {
		val gerundF:GerundForm = GerundForm(
			lang = Latin,
			surfaceForm = "disserendum",
			lemma = Some("dissero"),
			grammaticalCase = Nominative,
		)
		assert(gerundF.toString.trim == "**disserendum** (from dissero): gerund, neut., nom., sing.")
		assert( gerundF.toPos == "n-s---nn-")
	}

	it should "allow the creation of a GerundiveForm" in {
		val gerundF:GerundiveForm = GerundiveForm(
			lang = Latin,
			surfaceForm = "disserendi",
			lemma = Some("dissero"),
			grammaticalCase = Genitive,
			gender = Masculine,
			grammaticalNumber = Singular,
		)
		assert(gerundF.toString.trim == "**disserendi** (from dissero): gerundive, masc., gen., sing.")
		assert( gerundF.toPos == "a-s---mg-")
	}

	it should "allow the creation of a SupineForm" in {
		val supineF:SupineForm = SupineForm(
			lang = Latin,
			surfaceForm = "dictū",
			lemma = Some("dico"),
			grammaticalCase = Ablative,
		)
		assert(supineF.toString.trim == "**dictū** (from dico): supine, neut., abl., sing.")
		assert( supineF.toPos == "n-s---nb-")
	}

	 

}
