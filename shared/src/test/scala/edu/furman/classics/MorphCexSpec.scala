package edu.furman.classics.fumorph

import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import cats.syntax.either._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.scalatest.FlatSpec

class MorphCexSpec extends FlatSpec {

	"The MorphCex library" should "allow export of a FiniteVerbForm" in {
		val fvf:FiniteVerbForm = FiniteVerbForm(
			lang = Greek,
			surfaceForm = "λύομεν",
			lemma = Some("λύω"),
			person = First,
			grammaticalNumber = Plural,
			tense = Present,
			voice = Active,
			mood = Indicative,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, fvf, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow export of a Vector of  FiniteVerbForms" in {
		val fvf1:FiniteVerbForm = FiniteVerbForm(
			lang = Greek,
			surfaceForm = "λύομεν",
			lemma = Some("λύω"),
			person = First,
			grammaticalNumber = Plural,
			tense = Present,
			voice = Active,
			mood = Indicative,
			info = "some info"
		)
		val fvf2:FiniteVerbForm = FiniteVerbForm(
			lang = Greek,
			surfaceForm = "λύετε",
			lemma = Some("λύω"),
			person = Second,
			grammaticalNumber = Plural,
			tense = Present,
			voice = Active,
			mood = Indicative,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, Vector(fvf1,fvf2), "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an InvalidForm" in {
		val indeclf:InvalidForm = InvalidForm(
			lang = Greek,
			surfaceForm = "δέ",
			lemma = Some("δέ"),
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, indeclf, "fumorph", "temp", "#")

	}

	it should "allow the export of a vector of InvalidForms as a cex block" in {
		val indeclf1:InvalidForm = InvalidForm(
			lang = Greek,
			surfaceForm = "καί",
			lemma = None,
			info = "some info"
		)
		val indeclf2:InvalidForm = InvalidForm(
			lang = Greek,
			surfaceForm = "δέ",
			lemma = Some("δέ"),
			info = "some info"
		)
		val fv:Vector[InvalidForm] = Vector(indeclf1, indeclf2)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String  = fm.cex(Greek, fv, "fumorph", "temp", "#")
		println(cex)

	}

	it should "allow the export of an IndeclinableForm" in {
		val form:IndeclinableForm = IndeclinableForm(
			lang = Greek,
			surfaceForm = "δέ",
			lemma = Some("δέ"),
			pos = Particle,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of a NounForm" in {
		val form:NounForm = NounForm(
			lang = Greek,
			surfaceForm = "μῆνιν",
			lemma = Some("μῆνις"),
			gender = Feminine,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}
	 
	it should "allow the export of a AdjectiveForm" in {
		val form:AdjectiveForm = AdjectiveForm(
			lang = Greek,
			surfaceForm = "καλήν",
			lemma = Some("καλός"),
			gender = Feminine,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular,
			degree = Positive,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of a PronounForm" in {
		val form:PronounForm = PronounForm(
			lang = Greek,
			surfaceForm = "ἥν",
			lemma = Some("ὅς"),
			gender = Feminine,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an ArticleForm" in {
		val form:ArticleForm = ArticleForm(
			lang = Greek,
			surfaceForm = "τήν",
			lemma = Some("ὁ"),
			gender = Feminine,
			grammaticalCase = Accusative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an VerbalAdjectiveForm" in {
		val form:VerbalAdjectiveForm = VerbalAdjectiveForm(
			lang = Greek,
			surfaceForm = "διαβατέος",
			lemma = Some("διαβατέος"),
			gender = Masculine,
			grammaticalCase = Nominative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an GerundForm" in {
		val form:GerundForm = GerundForm(
			lang = Latin,
			surfaceForm = "disserendum",
			lemma = Some("dissero"),
			gender = Neuter,
			grammaticalCase = Nominative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Latin)
		val cex:String = fm.cex(Latin, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an GerundiveForm" in {
		val form:GerundiveForm = GerundiveForm(
			lang = Latin,
			surfaceForm = "disserendum",
			lemma = Some("dissero"),
			gender = Neuter,
			grammaticalCase = Nominative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an SupineForm" in {
		val form:SupineForm = SupineForm(
			lang = Latin,
			surfaceForm = "dictū",
			lemma = Some("dico"),
			gender = Neuter,
			grammaticalCase = Ablative,
			grammaticalNumber = Singular,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an AdverbForm" in {
		val form:AdverbForm = AdverbForm(
			lang = Greek,
			surfaceForm = "καλῶς",
			lemma = Some("καλῶς"),
			degree = Positive,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Greek, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of an InfinitiveForm" in {
		val form:InfinitiveForm = InfinitiveForm(
			lang = Latin,
			surfaceForm = "amare",
			lemma = Some("amo"),
			tense = Present,
			voice = Active,
			info = "some info"
		)
		val fm:MorphCex = new MorphCex(Latin)
		val cex:String = fm.cex(Latin, form, "fumorph", "temp", "#")
		println(cex)
	}

	it should "allow the export of a ParticipleForm" in {
		val form:ParticipleForm = ParticipleForm(lang = Greek, surfaceForm = "οἰκουμένην", lemma = Some("οἰκέω"), tense = Present, voice = Passive, gender = Feminine, grammaticalCase = Accusative, grammaticalNumber = Singular, info = "some info" ) 
		val fm:MorphCex = new MorphCex(Greek)
		val cex:String = fm.cex(Latin, form, "fumorph", "temp", "#")
		println(cex)
	}

}