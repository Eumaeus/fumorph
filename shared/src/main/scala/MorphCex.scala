package edu.furman.classics.fumorph
import scala.scalajs.js
import scala.scalajs.js.annotation._
import edu.holycross.shot.cite._
import edu.holycross.shot.greek._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._


@JSExportAll
case class MorphCex(lang:MorphLanguage) {

	def cexProps(formType:Serializable, urn:Cite2Urn, delimiter:String = "#"):String = {
		val header:String = "#!citeproperties\nProperty#Label#Type#Authority list"
		val props:Vector[String] = formType match {
			case InvalidForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case FiniteVerbForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "person", "number", "tense", "voice", "mood", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case IndeclinableForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "pos", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case NounForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case AdjectiveForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "degree", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case PronounForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case ArticleForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case AdverbForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "degree", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case InfinitiveForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "tense", "voice", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case ParticipleForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "tense", "voice", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case VerbalAdjectiveForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case GerundForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case GerundiveForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case SupineForm => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "gender", "case", "number", "info")
				val urnRec:String = s"""${urn.addProperty("urn")}${delimiter}urn${delimiter}String${delimiter}"""
				val others:Vector[String] = fields.tail.map(f => {
					val purn:Cite2Urn = urn.addProperty(f)
					s"${purn}${delimiter}${f}${delimiter}String${delimiter}"
				})
				( Vector(header) ++ Vector(urnRec) ++ others )
			}
			case _ => {
				val fields:Vector[String] = Vector("urn", "label", "surfaceform", "lemma", "lang", "info")
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
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}info"
			}
			case FiniteVerbForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}person${delimiter}number${delimiter}tense${delimiter}voice${delimiter}mood${delimiter}info"
			}
			case IndeclinableForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}pos${delimiter}info"
			}
			case NounForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case AdjectiveForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}degree${delimiter}info"
			}
			case PronounForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case ArticleForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case AdverbForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}degree${delimiter}info"
			}
			case InfinitiveForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}tense${delimiter}voice${delimiter}info"
			}
			case ParticipleForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}tense${delimiter}voice${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case VerbalAdjectiveForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case GerundForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case GerundiveForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case SupineForm => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}gender${delimiter}case${delimiter}number${delimiter}info"
			}
			case _ => {
				s"urn${delimiter}label${delimiter}surfaceform${delimiter}lemma${delimiter}lang${delimiter}info"
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

		val catalog:Vector[String] = {
			val colls:Vector[String] = {
				groupVec.map( g => {
					g._2.head.typeName
				})
			}
			cexGetCatalog(colls, nameSpace, versionName, delimiter)
		}

		val dataModels:Vector[String] = {
			val colls:Vector[String] = {
				groupVec.map( g => {
					g._2.head.typeName
				})
			}
			getCexDataModels(lang, colls, nameSpace, versionName, delimiter)
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
				val urn:Cite2Urn = UrnGenerator.get(nameSpace, f.typeName, versionName, gv._2.indexOf(f))
				var newForm:Form = f
				formCex(formType, f, urn, delimiter)
			})
			( Vector(header) ++ dataVec ++ Vector("\n"))
		}).flatten

		( Vector(cexBoilerplate(delimiter)) ++ catalog ++ dataModels ++ cexProperties ++ cexVec ).mkString("\n")
	}

	def formCex(formType:Serializable, form:Form, urn:Cite2Urn, delimiter:String = "#"):String = {
		formType match {
			case InvalidForm => {
				val f:InvalidForm = form.asInstanceOf[InvalidForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.info}"""
			}
			case FiniteVerbForm => {
				val f:FiniteVerbForm = form.asInstanceOf[FiniteVerbForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.person}${delimiter}${f.grammaticalNumber}${delimiter}${f.tense}${delimiter}${f.voice}${delimiter}${f.mood}${delimiter}${f.info}"""
			}
			case IndeclinableForm => {
				val f:IndeclinableForm = form.asInstanceOf[IndeclinableForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.pos}${delimiter}${f.info}"""
			}
			case NounForm => {
				val f:NounForm = form.asInstanceOf[NounForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case AdjectiveForm => {
				val f:AdjectiveForm = form.asInstanceOf[AdjectiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.degree}${delimiter}${f.info}"""
			}
			case PronounForm => {
				val f:PronounForm = form.asInstanceOf[PronounForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case ArticleForm => {
				val f:ArticleForm = form.asInstanceOf[ArticleForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case AdverbForm => {
				val f:AdverbForm = form.asInstanceOf[AdverbForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.degree}${delimiter}${f.info}"""
			}
			case InfinitiveForm => {
				val f:InfinitiveForm = form.asInstanceOf[InfinitiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.tense}${delimiter}${f.voice}${delimiter}${f.info}"""
			}
			case ParticipleForm => {
				val f:ParticipleForm = form.asInstanceOf[ParticipleForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.tense}${delimiter}${f.voice}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case VerbalAdjectiveForm => {
				val f:VerbalAdjectiveForm = form.asInstanceOf[VerbalAdjectiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case GerundForm => {
				val f:GerundForm = form.asInstanceOf[GerundForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case GerundiveForm => {
				val f:GerundiveForm = form.asInstanceOf[GerundiveForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case SupineForm => {
				val f:SupineForm = form.asInstanceOf[SupineForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.gender}${delimiter}${f.grammaticalCase}${delimiter}${f.grammaticalNumber}${delimiter}${f.info}"""
			}
			case _ => {
				val f:InvalidForm = form.asInstanceOf[InvalidForm]
				s"""${urn}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifySurfaceForm(f.lang, f.surfaceForm)}${delimiter}${greekifyLemma(f.lang, f.lemma)}${delimiter}${f.lang.abbr}${delimiter}${f.info}"""
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

	def formsFromCiteObjects(lib:CiteLibrary, lang:MorphLanguage = lang):Vector[CitableMorphology] = {
		println(s"Got here: formsFromCiteObjects")
		lib.collectionRepository match {
			case Some(cr) => {
				// Get collections for each category
					val objsInvalidForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:InvalidForm"))
						println(s"\n\ncolls:\n\n${colls}\n\n")
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, InvalidForm(lang, surfaceForm, lemma, info))
					 	})
					}
					
					val objsFiniteVerbForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:FiniteVerbForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val person:String = obj.propertyValue(u.addProperty("person")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val tense:String = obj.propertyValue(u.addProperty("tense")).asInstanceOf[String]
					 		val voice:String = obj.propertyValue(u.addProperty("voice")).asInstanceOf[String]
					 		val mood:String = obj.propertyValue(u.addProperty("mood")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, FiniteVerbForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(person).asInstanceOf[Person], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			propertyFromString(tense).asInstanceOf[Tense], 
					 			propertyFromString(mood).asInstanceOf[Mood], 
					 			propertyFromString(voice).asInstanceOf[Voice], 
					 			info))
					 	})
					}

					val objsIndeclinableForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:IndeclinableForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val pos:String = obj.propertyValue(u.addProperty("pos")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, IndeclinableForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(pos).asInstanceOf[IndeclinablePoS], 
					 			info))
					 	})
					}

					val objsNounForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:NounForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, NounForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsAdjectiveForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:AdjectiveForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val degree:String = obj.propertyValue(u.addProperty("degree")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, AdjectiveForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			propertyFromString(degree).asInstanceOf[Degree], 
					 			info))
					 	})
					}		

					val objsPronounForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:PronounForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, PronounForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsArticleForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:ArticleForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, ArticleForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsAdverbForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:AdverbForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val degree:String = obj.propertyValue(u.addProperty("degree")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, AdverbForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(degree).asInstanceOf[Degree], 
					 			info))
					 	})
					}

					val objsInfinitiveForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:InfinitiveForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val tense:String = obj.propertyValue(u.addProperty("tense")).asInstanceOf[String]
					 		val voice:String = obj.propertyValue(u.addProperty("voice")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, InfinitiveForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(tense).asInstanceOf[Tense], 
					 			propertyFromString(voice).asInstanceOf[Voice], 
					 			info))
					 	})
					}

					val objsParticipleForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:ParticipleForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val tense:String = obj.propertyValue(u.addProperty("tense")).asInstanceOf[String]
					 		val voice:String = obj.propertyValue(u.addProperty("voice")).asInstanceOf[String]
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, ParticipleForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(tense).asInstanceOf[Tense], 
					 			propertyFromString(voice).asInstanceOf[Voice], 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsVerbalAdjectiveForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:VerbalAdjectiveForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, VerbalAdjectiveForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsGerundForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:GerundForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, GerundForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsGerundiveForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:GerundiveForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, GerundiveForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					val objsSupineForm:Vector[CitableMorphology] = {
						val colls:Vector[Cite2Urn] = lib.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:SupineForm"))
					 	val urns:Vector[Cite2Urn] = colls.map( c => {
					 		cr.collectionsMap(c)
					 	}).flatten
					 	urns.map( u => {
					 		val obj:CiteObject = cr.citableObject(u)
					 		val lang:MorphLanguage = {
					 			obj.propertyValue(u.addProperty("lang")).asInstanceOf[String] match {
					 				case "grc" => Greek
					 				case _ => Latin
					 			}
					 		}
					 		val surfaceForm:String = obj.propertyValue(u.addProperty("surfaceform")).asInstanceOf[String]
					 		val lemma:Option[String] = obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String] match {
					 			case "" => None
					 			case _ => Some(obj.propertyValue(u.addProperty("lemma")).asInstanceOf[String])
					 		}
					 		val gender:String = obj.propertyValue(u.addProperty("gender")).asInstanceOf[String]
					 		val grammaticalCase:String = obj.propertyValue(u.addProperty("case")).asInstanceOf[String]
					 		val grammaticalNumber:String = obj.propertyValue(u.addProperty("number")).asInstanceOf[String]
					 		val info:String = obj.propertyValue(u.addProperty("info")).asInstanceOf[String]
					 		CitableMorphology(u, SupineForm(
					 			lang, 
					 			surfaceForm, 
					 			lemma, 
					 			propertyFromString(gender).asInstanceOf[Gender], 
					 			propertyFromString(grammaticalCase).asInstanceOf[GrammaticalCase], 
					 			propertyFromString(grammaticalNumber).asInstanceOf[GrammaticalNumber], 
					 			info))
					 	})
					}

					objsInvalidForm ++ objsFiniteVerbForm ++ objsIndeclinableForm ++ objsNounForm ++ objsAdjectiveForm ++ objsPronounForm ++ objsArticleForm ++ objsAdverbForm ++ objsInfinitiveForm ++ objsParticipleForm ++ objsVerbalAdjectiveForm ++ objsGerundForm ++ objsGerundiveForm ++ objsSupineForm
			}
			case None => {
				Vector[CitableMorphology]()
			}
		}
	}

	def greekifySurfaceForm(lang:MorphLanguage, s:String):String = {
		if (lang == Greek) {
			val lgs:LiteraryGreekString = LiteraryGreekString(s.replaceAll("%27","'"))
			lgs.ucode.replaceAll("σ$","ς")
		} else {
			s
		}
	}

	def greekifyLemma(lang:MorphLanguage, os:Option[String]):String = {
		os match {
			case Some(s) => {
				if (lang == Greek) {
					val lgs:LiteraryGreekString = LiteraryGreekString(s.replaceAll("%27","'"))
					lgs.ucode.replaceAll("σ$","ς")
				} else {
					s
				}
			}
			case None => ""
		}
		
	}

	def getCexDataModels(lang:MorphLanguage, colls:Vector[String], nameSpace:String, versionName:String, delimiter:String):Vector[String] = {
		val header:Vector[String] = Vector(s"""#!datamodels
Collection${delimiter}Model${delimiter}Label${delimiter}Description
// Morphology collections""")
		val collsCex:Vector[String] = colls.map( c => {
			val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}:")
			val theRest:String = s"${delimiter}urn:cite2:cite:datamodels.v1:fumorph${delimiter}FU Morphology${delimiter}Morphological data."
			s"${urn}${theRest}"
		})

		val typeDMs:Vector[String] = {
			val comment:Vector[String] = Vector("// Morph Type Implementations")
			val typePropsCex:Vector[String] = colls.map( c => {
					val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}:")
					val dmUrn:Cite2Urn = Cite2Urn(s"urn:cite2:cite:datamodels.v1:${c}")
					val theRest:String = s"""${delimiter}${dmUrn}${delimiter}${c}${delimiter}Furman University Morphology Collection: ${c}."""
					Vector(s"${urn}${theRest}")
				}).flatten
				comment ++ typePropsCex
		}

		val lgsDMs:Vector[String] = {
			if (lang == Greek) {
				val comment:Vector[String] = Vector("// Greek Fields")
				val lgsPropsCex:Vector[String] = colls.map( c => {
					val sf:String = {
						val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}.surfaceform:")
						urn.toString
					}
					val lem:String = {
						val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}.lemma:")
						urn.toString
					}
					val theRest:String = s"""${delimiter}urn:cite2:cite:datamodels.v1:lgs${delimiter}LiteraryGreekString${delimiter}Text implements LiteraryGreekString."""
					Vector(s"${sf}${theRest}", s"${lem}${theRest}")
				}).flatten
				comment ++ lgsPropsCex
			} else {
				Vector[String]()
			}
		}

		val boilerPlate:Vector[String] = {
			Vector(cexDataModelsBoilerplate(delimiter))
		}
		(header ++ collsCex ++ typeDMs ++ lgsDMs ++ boilerPlate)
	}

	def cexGetCatalog(colls:Vector[String], nameSpace:String, versionName:String, delimiter:String):Vector[String] = {
		val header:String = "#!citecollections\nURN#Description#Labelling property#Ordering property#License"
		val collsCex:Vector[String] = colls.map( c => {
			val urn:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}:")
			val desc:String = s"Generated collection of ${c}s"
			val labelProp:Cite2Urn = Cite2Urn(s"urn:cite2:${nameSpace}:${c}.${versionName}.label:")
			val license:String = "Public Domain"
			s"${urn}${delimiter}${desc}${delimiter}${labelProp}${delimiter}${delimiter}${license}"
		})
		( Vector(header) ++ collsCex ++ Vector("\n"))
	}

	def cexPropertyHeader(delimiter:String = "#"):String = s"#!citeproperties\nProperty${delimiter}Label${delimiter}Type${delimiter}Authority list"

	def cexBoilerplate(delimiter:String = "#"):String = s"""#!cexversion
3.0

#!citelibrary
name${delimiter}CEX Library created by FuMorph
urn${delimiter}urn:cite2:cex:TEMPCOLL.TEMPVERSION:TEMP_ID
license${delimiter}Public Domain.

"""

	def cexDataModelsBoilerplate(delimiter:String = "#"):String = s"""\n#!citecollections
URN${delimiter}Description${delimiter}Labelling property${delimiter}Ordering property${delimiter}License
urn:cite2:cite:datamodels.v1:${delimiter}CITE data models${delimiter}urn:cite2:cite:datamodels.v1.label:${delimiter}${delimiter}Public domain

#!citeproperties
Property${delimiter}Label${delimiter}Type${delimiter}Authority list
urn:cite2:cite:datamodels.v1.urn:${delimiter}Data model${delimiter}Cite2Urn${delimiter}
urn:cite2:cite:datamodels.v1.label:${delimiter}Label${delimiter}String${delimiter}
urn:cite2:cite:datamodels.v1.description:${delimiter}Description${delimiter}String${delimiter}

#!citedata
urn${delimiter}label${delimiter}description
urn:cite2:cite:datamodels.v1:fumorph${delimiter}F.U. Morphology Collection${delimiter}Model of morphology data for Latin and Greek.
// Specific morphological collections
urn:cite2:cite:datamodels.v1:ArticleForm${delimiter}ArticleForm${delimiter}Furman University Morphology Collection: Articles.
urn:cite2:cite:datamodels.v1:InvalidForm${delimiter}Invalid${delimiter}Furman University Morphology Collection: Invalid.
urn:cite2:cite:datamodels.v1:FiniteVerbForm${delimiter}FiniteVerb${delimiter}Furman University Morphology Collection: FiniteVerb.
urn:cite2:cite:datamodels.v1:IndeclinableForm${delimiter}Indeclinable${delimiter}Furman University Morphology Collection: Indeclinable.
urn:cite2:cite:datamodels.v1:NounForm${delimiter}Noun${delimiter}Furman University Morphology Collection: Noun.
urn:cite2:cite:datamodels.v1:AdjectiveForm${delimiter}Adjective${delimiter}Furman University Morphology Collection: Adjective.
urn:cite2:cite:datamodels.v1:PronounForm${delimiter}Pronoun${delimiter}Furman University Morphology Collection: Pronoun.
urn:cite2:cite:datamodels.v1:AdverbForm${delimiter}Adverb${delimiter}Furman University Morphology Collection: Adverb.
urn:cite2:cite:datamodels.v1:InfinitiveForm${delimiter}Infinitive${delimiter}Furman University Morphology Collection: Infinitive.
urn:cite2:cite:datamodels.v1:ParticipleForm${delimiter}Participle${delimiter}Furman University Morphology Collection: Participle.
urn:cite2:cite:datamodels.v1:VerbalAdjectiveForm${delimiter}VerbalAdjective${delimiter}Furman University Morphology Collection: VerbalAdjective.
urn:cite2:cite:datamodels.v1:GerundForm${delimiter}Gerund${delimiter}Furman University Morphology Collection: Gerund.
urn:cite2:cite:datamodels.v1:GerundiveForm${delimiter}Gerundive${delimiter}Furman University Morphology Collection: Gerundive.
urn:cite2:cite:datamodels.v1:SupineForm${delimiter}Supine${delimiter}Furman University Morphology Collection: Supine.
// literary Greek string
urn:cite2:cite:datamodels.v1:lgs${delimiter}Literary Greek String${delimiter}Identifies CITE Properties that contain representations of the LiteraryGreekString class.\n"""
	

	def propertyFromString(s:String):MorphologicalProperty = {
		s match {
			case "nominative" => { Nominative }
			case "genitive" => { Genitive }
			case "dative" => { Dative }
			case "accusative" => { Accusative }
			case "vocative" => { Vocative }
			case "locative" => { Locative }
			case "ablative" => { Ablative }
			case "masculine" => { Masculine }
			case "feminine" => { Feminine }
			case "neuter" => { Neuter }
			case "singular" => { Singular }
			case "dual" => { Dual }
			case "plural" => { Plural }
			case "1st person" => { First }
			case "1st" => { First }
			case "2nd person" => { Second }
			case "2nd" => { Second }
			case "3rd person" => { Third }
			case "3rd" => { Third }
			case "present" => { Present }
			case "imperfect" => { Imperfect }
			case "future" => { Future }
			case "aorist" => { Aorist }
			case "perfect" => { Perfect }
			case "pluperfect" => { Pluperfect }
			case "future perfect" => { FuturePerfect }
			case "indicative" => { Indicative }
			case "subjunctive" => { Subjunctive }
			case "optative" => { Optative }
			case "imperative" => { Imperative }
			case "positive" => { Positive }
			case "comparative" => { Comparative }
			case "superlative" => { Superlative }
			case "active" => { Active }
			case "middle" => { Middle }
			case "passive" => { Passive }
			case "medio-passive" => { MedioPassive }
			case "deponent" => { Deponent }
			case "conjunction" => { Conjunction }
			case "preposition" => { Preposition }
			case "interjection" => { Interjection }
			case _ => { Unmatched }
		}
	}

}

