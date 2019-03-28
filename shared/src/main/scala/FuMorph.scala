package edu.furman.classics.fumorph
import scala.scalajs.js
import scala.scalajs.js.annotation._
import edu.holycross.shot.cite._
import edu.holycross.shot.greek._

@JSExportAll
case class FuMorph(lang:MorphLanguage) {

	def cexProps(formType:Serializable, urn:Cite2Urn, delimiter:String = "#"):String = {
		val header:String = "#!citeproperties\nProperty#Label#Type#Authority list"
		val props:Vector[String] = formType match {
			case InvalidForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case FiniteVerbForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "person", "number", "tense", "voice", "mood", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case IndeclinableForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "pos", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case NounForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case AdjectiveForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "degree", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case PronounForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case ArticleForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case AdverbForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "degree", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case InfinitiveForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "tense", "voice", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case ParticipleForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "tense", "voice", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case VerbalAdjectiveForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case GerundForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case GerundiveForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case SupineForm => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case _ => {
				val fields:Vector[String] = Vector("urn", "surfaceform", "lemma", "lang", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
		}
		props.mkString("\n") + "\n"
	}

	def cexHeader(formType:Serializable, delimiter:String = "#"):String = {
		val header:String = "#!citedata"
		val fields:String = formType match {
			case InvalidForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}info"
			}
			case FiniteVerbForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}person${delimiter}number${delimiter}tense${delimiter}voice${delimiter}mood${delimiter}info"
			}
			case IndeclinableForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}pos${delimiter}info"
			}
			case NounForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case AdjectiveForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}degree${delimiter}info"
			}
			case PronounForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case ArticleForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case AdverbForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}degree${delimiter}info"
			}
			case InfinitiveForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}tense${delimiter}voice${delimiter}info"
			}
			case ParticipleForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}tense${delimiter}voice${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case VerbalAdjectiveForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case GerundForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case GerundiveForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case SupineForm => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case _ => {
				s"urn${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}info"
			}
		}

		header + "\n" + fields
	}

	def groupForms(formVec:Vector[Form]):Vector[(Serializable, Vector[Form])] = {
		formVec.distinct.map( mf => {
			mf match {
				case _:InvalidForm => (InvalidForm, mf)
				case _:FiniteVerbForm => (FiniteVerbForm, mf)
				case _:IndeclinableForm => (IndeclinableForm, mf)
				case _:NounForm => (NounForm, mf)
				case _:AdjectiveForm => (AdjectiveForm, mf)
				case _:PronounForm => (PronounForm, mf)
				case _:ArticleForm => (ArticleForm, mf)
				case _:AdverbForm => (AdverbForm, mf)
				case _:InfinitiveForm => (InfinitiveForm, mf)
				case _:ParticipleForm => (ParticipleForm, mf)
				case _:VerbalAdjectiveForm => (VerbalAdjectiveForm, mf)
				case _:GerundForm => (GerundForm, mf)
				case _:GerundiveForm => (GerundiveForm, mf)
				case _:SupineForm => (SupineForm, mf)
				case _ => (InvalidForm, mf)
			}
		}).groupBy(_._1).toVector.map( t => {
			(t._1, t._2.map(_._2))
		})	
	}



	def cex(lang:MorphLanguage, form:Form, nameSpace:String, versionName:String, delimiter:String):String = {
		cex(lang, Vector(form), nameSpace, versionName, delimiter)
	}
	
	def cex(lang:MorphLanguage, formVec:Vector[Form], nameSpace:String = "fumorph", versionName:String = "temp", delimiter:String = "#"):String = {

		val groupVec:Vector[(Serializable, Vector[Form])] = groupForms(formVec)

		val catalog:String = {
			val colls:Vector[String] = {
				groupVec.map( g => {
					g._2.head.typeName
				})
			}
			cexGetCatalog(colls, nameSpace, versionName, delimiter)
		}

		val cexProperties:Vector[String] = {
			groupVec.map( g => {
				val typeName:String = g._2.head.typeName
				val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${typeName}.${versionName}:")
				cexProps(g._1, urn, delimiter)
			})
		}

		val cexVec:Vector[String] = groupVec.map( gv => {
			val formType:Serializable = gv._1
			val header:String = cexHeader(formType, delimiter)
			val dataVec:Vector[String] = gv._2.map( f => {
				val urn:Cite2Urn = UrnGenerator.get(nameSpace, f.typeName, versionName)
				var newForm:Form = f
				formCex(formType, f, urn, delimiter)
			})
			( Vector(header) ++ dataVec ++ Vector("\n"))
		}).flatten

		( Vector(cexBoilerplate(delimiter)) ++ Vector(catalog) ++ cexProperties ++ cexVec ).mkString("\n")
	}

	def formCex(formType:Serializable, form:Form, urn:Cite2Urn, delimiter:String = "#"):String = {
		formType match {
			case InvalidForm => {
				val f:InvalidForm = form.asInstanceOf[InvalidForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.info}"""
			}
			case FiniteVerbForm => {
				val f:FiniteVerbForm = form.asInstanceOf[FiniteVerbForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.person}${delimiter}${f.grammaticalNumber}${delimiter}${f.tense}${delimiter}${f.voice}${delimiter}${f.mood}${delimiter}${f.info}"""
			}
			case IndeclinableForm => {
				val f:IndeclinableForm = form.asInstanceOf[IndeclinableForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.pos}${delimiter}${f.info}"""
			}
			case NounForm => {
				val f:NounForm = form.asInstanceOf[NounForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case AdjectiveForm => {
				val f:AdjectiveForm = form.asInstanceOf[AdjectiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.degree}${delimiter}${f.info}"""
			}
			case PronounForm => {
				val f:PronounForm = form.asInstanceOf[PronounForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case ArticleForm => {
				val f:ArticleForm = form.asInstanceOf[ArticleForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case AdverbForm => {
				val f:AdverbForm = form.asInstanceOf[AdverbForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.degree}${delimiter}${f.info}"""
			}
			case InfinitiveForm => {
				val f:InfinitiveForm = form.asInstanceOf[InfinitiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.tense}${delimiter}${f.voice}${delimiter}${f.info}"""
			}
			case ParticipleForm => {
				val f:ParticipleForm = form.asInstanceOf[ParticipleForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.tense}${delimiter}${f.voice}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case VerbalAdjectiveForm => {
				val f:VerbalAdjectiveForm = form.asInstanceOf[VerbalAdjectiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case GerundForm => {
				val f:GerundForm = form.asInstanceOf[GerundForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case GerundiveForm => {
				val f:GerundiveForm = form.asInstanceOf[GerundiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case SupineForm => {
				val f:SupineForm = form.asInstanceOf[SupineForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case _ => {
				val f:InvalidForm = form.asInstanceOf[InvalidForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(lang, f.surfaceForm)}${delimiter}${greekifyLemma(lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.info}"""
			}
		}
	}
	

	def analysisType(form:Form):Serializable = {
		form match {
			case _:InvalidForm => InvalidForm
			case _:FiniteVerbForm => FiniteVerbForm
			case _:IndeclinableForm => IndeclinableForm
			case _:NounForm => NounForm
			case _:AdjectiveForm => AdjectiveForm
			case _:PronounForm => PronounForm
			case _:ArticleForm => ArticleForm
			case _:AdverbForm => AdverbForm
			case _:InfinitiveForm => InfinitiveForm
			case _:ParticipleForm => ParticipleForm
			case _:VerbalAdjectiveForm => VerbalAdjectiveForm
			case _:GerundForm => GerundForm
			case _:GerundiveForm => GerundiveForm
			case _:SupineForm => SupineForm
			case _ => InvalidForm
		}
	}

	def greekifySurfaceForm(lang:MorphLanguage, s:String):String = {
		if (lang == Greek) {
			val lgs:LiteraryGreekString = LiteraryGreekString(s)
			lgs.ascii
		} else {
			s
		}
	}

	def greekifyLemma(lang:MorphLanguage, os:Option[String]):String = {
		os match {
			case Some(s) => {
				if (lang == Greek) {
					val lgs:LiteraryGreekString = LiteraryGreekString(s)
					lgs.ascii
				} else {
					s
				}
			}
			case None => ""
		}
		
	}

	def cexGetCatalog(colls:Vector[String], nameSpace:String, versionName:String, delimiter:String):String = {
		val header:String = "#!citecollections\nURN#Description#Labelling property#Ordering property#License"
		val collsCex:Vector[String] = colls.map( c => {
			val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}:")
			val desc:String = s"Generated collection of ${c}s"
			val labelProp:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}.surfaceform:")
			val license:String = "Public Domain"
			s"${urn}${delimiter}${desc}${delimiter}${labelProp}${delimiter}${delimiter}${license}"
		})
		( Vector(header) ++ collsCex ++ Vector("\n")).mkString("\n")
	}

	def cexPropertyHeader(delimiter:String = "#"):String = s"#!citeproperties\nProperty${delimiter}Label${delimiter}Type${delimiter}Authority list"

	def cexBoilerplate(delimiter:String = "#"):String = s"""#!cexversion
3.0

#!citelibrary
name${delimiter}CEX Library created by FuMorph
urn${delimiter}urn:cite2:cex:TEMPCOLL.TEMPVERSION:TEMP_ID
license${delimiter}Public Domain.

"""
	
}

