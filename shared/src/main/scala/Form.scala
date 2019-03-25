package edu.furman.classics.fumorph

sealed trait Form {
	val surfaceForm:String
	val lemma:Option[String]
	val lang:MorphLanguage
	val info:String
}

trait DeclinableForm {
	val gender: Gender
	val grammaticalCase: GrammaticalCase
	val grammaticalNumber: GrammaticalNumber
}

trait ConjugatableForm{
	val tense: Tense 
	val voice: Voice
}

case class FiniteVerbForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], person: Person, grammaticalNumber: GrammaticalNumber, tense: Tense, mood: Mood, voice: Voice, info:String = "") extends Form with ConjugatableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: finite verb, ${person.abbr} ${grammaticalNumber.abbr}, ${tense.abbr} ${voice.abbr} ${mood.abbr} ${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"v${person.short}${grammaticalNumber.short}${tense.short}${voice.short}${mood.short}---"
	}
}

case class IndeclinableForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], pos: IndeclinablePoS, info:String = "") extends Form {

	override def toString = {
		s"""**${surfaceForm}**: ${pos.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		"d--------"
	}

}

case class NounForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: noun, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"n-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

}

case class AdjectiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree, info:String = "") extends Form with DeclinableForm {
}

case class PronounForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree, info:String = "") extends Form with DeclinableForm {
}

case class ArticleForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree, info:String = "") extends Form with DeclinableForm {
}

case class AdverbForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], degree: Degree = Positive, info:String = "") extends Form {
}

case class InfinitiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], tense:Tense, voice:Voice, info:String = "") extends Form with ConjugatableForm{
}

case class ParticipleForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], tense:Tense, voice:Voice, gender:Gender, grammaticalCase:GrammaticalCase, grammaticalNumber:GrammaticalNumber, info:String = "") extends Form with ConjugatableForm with DeclinableForm {
}

case class VerbalAdjectiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form  with DeclinableForm {
}

case class GerundForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form  with DeclinableForm {
}

case class GerundiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {
}

case class SupineForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {
}
