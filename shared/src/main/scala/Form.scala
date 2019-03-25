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

case class AdjectiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree = Positive, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: adjective, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr} (${degree.abbr} degree)${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"a-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}${degree.short}"
	}

}

case class PronounForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: pronoun, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"p-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}
}

case class ArticleForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: article, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"l-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}
}

case class AdverbForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], degree: Degree = Positive, info:String = "") extends Form {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: adverb (${degree.abbr} degree)${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"d-------${degree.short}"
	}

}

case class InfinitiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], tense:Tense, voice:Voice, info:String = "") extends Form with ConjugatableForm{

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: ${tense.abbr} ${voice.abbr} infinitive${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"v--${tense.short}${voice.short}n---"
	}
}

case class ParticipleForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], tense:Tense, voice:Voice, gender:Gender, grammaticalCase:GrammaticalCase, grammaticalNumber:GrammaticalNumber, info:String = "") extends Form with ConjugatableForm with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: ${tense.abbr} ${voice.abbr} participle, ${gender.abbr} ${grammaticalCase.abbr} ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"v-${grammaticalNumber.short}${tense.short}${voice.short}p${gender.short}${grammaticalCase.short}-"
	}
}

case class VerbalAdjectiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form  with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: verbal adjective, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"a-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}
}

case class GerundForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender = Neuter, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber = Singular, info:String = "") extends Form  with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: gerund, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"n-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}
}

case class GerundiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: gerundive, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"a-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

}

case class SupineForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender = Neuter, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber = Singular, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: supine, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"n-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}
}
