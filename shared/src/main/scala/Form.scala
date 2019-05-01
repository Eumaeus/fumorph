package edu.furman.classics.fumorph

import edu.holycross.shot.cite._

abstract class Form {
	val surfaceForm:String
	val lemma:Option[String]
	val lang:MorphLanguage
	val info:String
	val typeName:String
	val html:String
}

trait DeclinableForm {
	val gender: Gender
	val grammaticalCase: GrammaticalCase
	val grammaticalNumber: GrammaticalNumber
}

trait ConjugatableForm {
	val tense: Tense 
	val voice: Voice
}

trait DegreeableForm {
	val degree: Degree
}

case class CitableMorphology(urn:Cite2Urn, form:Form) {
	val html:String = s"""
	<div class="fumorph_citableMorphology" data-morph-urn="${urn}">
		${form.html}
	</div>
	"""
}

case class InvalidForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], info:String = "") extends Form {
	override def toString = {
		s"""**Invalid Form** "${surfaceForm}" ${if (lemma != None) " (from " + lemma.get + ")" else ""}: ${if (info.size > 0) " " + info + "." else ""}"""
	}

	def toPos = {
		s"x--------"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "InvalidForm"

}

case class FiniteVerbForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], person: Person, grammaticalNumber: GrammaticalNumber, tense: Tense, mood: Mood, voice: Voice, info:String = "") extends Form with ConjugatableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: finite verb, ${person.abbr} ${grammaticalNumber.abbr}, ${tense.abbr} ${voice.abbr} ${mood.abbr} ${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"v${person.short}${grammaticalNumber.short}${tense.short}${voice.short}${mood.short}---"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">finite verb</span>
			<span class="fumorph_parse">${person.abbr} ${grammaticalNumber.abbr}, ${tense.abbr} ${voice.abbr} ${mood.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "FiniteVerbForm"

}

case class IndeclinableForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], pos: IndeclinablePoS, info:String = "") extends Form {

	override def toString = {
		s"""**${surfaceForm}**: ${pos.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"${pos.short}--------"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">indeclinable form</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "IndeclinableForm"
}

case class NounForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: noun, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"n-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">noun</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "NounForm"

}

case class AdjectiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree = Positive, info:String = "") extends Form with DeclinableForm with DegreeableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: adjective, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr} (${degree.abbr} degree)${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"a-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}${degree.short}"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">adjective</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "AdjectiveForm"

}

case class PronounForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: pronoun, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"p-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">pronoun</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "PronounForm"
}

case class ArticleForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: article, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"l-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">article</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "ArticleForm"
}

case class AdverbForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], degree: Degree = Positive, info:String = "") extends Form with DegreeableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: adverb (${degree.abbr} degree)${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"d-------${degree.short}"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">adverb</span>
			<span class="fumorph_parse">${degree.abbr} degree</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "AdverbForm"

}

case class InfinitiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], tense:Tense, voice:Voice, info:String = "") extends Form with ConjugatableForm{

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: ${tense.abbr} ${voice.abbr} infinitive${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"v--${tense.short}${voice.short}n---"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">infinitive</span>
			<span class="fumorph_parse">${tense.abbr} ${voice.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "InfinitiveForm"
}

case class ParticipleForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], tense:Tense, voice:Voice, gender:Gender, grammaticalCase:GrammaticalCase, grammaticalNumber:GrammaticalNumber, info:String = "") extends Form with ConjugatableForm with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: ${tense.abbr} ${voice.abbr} participle, ${gender.abbr} ${grammaticalCase.abbr} ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"v-${grammaticalNumber.short}${tense.short}${voice.short}p${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">participle</span>
			<span class="fumorph_parse">${tense.abbr} ${voice.abbr} participle, ${gender.abbr} ${grammaticalCase.abbr} ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "ParticipleForm"
}

case class VerbalAdjectiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form  with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: verbal adjective, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"a-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">verbal adjective</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "VerbalAdjectiveForm"
}

case class GerundForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender = Neuter, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber = Singular, info:String = "") extends Form  with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: gerund, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"n-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">gerund</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "GerundForm"
}

case class GerundiveForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: gerundive, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"a-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">gerundive</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "GerundiveForm"

}

case class SupineForm(lang:MorphLanguage, surfaceForm:String, lemma:Option[String], gender: Gender = Neuter, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber = Singular, info:String = "") extends Form with DeclinableForm {

	override def toString = {
		s"""**${surfaceForm}**${if (lemma != None) " (from " + lemma.get + ")" else ""}: supine, ${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}${if (info.size > 0) " (" + info + ".)" else ""}"""
	}

	def toPos = {
		s"n-${grammaticalNumber.short}---${gender.short}${grammaticalCase.short}-"
	}

	val html:String = {
		val lemmaStr:String ={
			lemma match {
				case Some(l) => s"""<span class="fumorph_lemma">${l}</span>"""
				case None => ""
			}
		}
		s"""
		<div class="fumorph_form">
			<span class="fumorph_surfaceForm">${this.surfaceForm.toString}</span>
			${lemmaStr}
			<span class="fumorph_formType">supine</span>
			<span class="fumorph_parse">${gender.abbr}, ${grammaticalCase.abbr}, ${grammaticalNumber.abbr}</span>
			<span class="fumorph_info">${info}</span>
		</div>
		"""
	}

	val typeName:String = "SupineForm"
}
