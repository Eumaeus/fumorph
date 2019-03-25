package edu.furman.classics.fumorph

import edu.holycross.shot.cite._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._
import cats.syntax.either._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.scalatest.FlatSpec

class PersMorphJsonSpec extends FlatSpec {

	"A PerseusMorphService Class" should  "return a valid URL" in {
		val tok:String = "oi("
		val lang:MorphLanguage = Greek 
		val mo:PerseusMorphService = PerseusMorphService(tok, lang)
		val expected:String = "http://services.perseids.org/bsp/morphologyservice/analysis/word?lang=grc&engine=morpheusgrc&mode=xml&word=oi("
		assert(mo.url == expected)
	}

	"A PerseusMorphJson Class" should "parse a JSON string" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(test1, Greek, "oi(")
		assert (pmj.doc != Json.Null)
	}

	
	it should "fail on bad JSON" in {
		val expected:String = """PerseusMorphJson Exception: Invalid JSON associated with "oi(" [grc]: < not json >"""
		try {
	      val pmj:PerseusMorphJson = PerseusMorphJson("not json", Greek, "oi(")
	      fail("Should not have made PerseusMorphJson")
	    } catch {
	      case mje: MorphJsonException => assert(mje.getMessage() == expected)
	      case t: Throwable => fail("Should have thrown an MorphJsonException Exception: " + s""" ${expected} """)
	    }
	}

	it should "include language and the tested surface form" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(test5, Greek, "xrh=n")
		assert( pmj.lang == Greek)
		assert( pmj.surfaceForm == "xrh=n")
	}

	it should "include an Option[Vector[Entries]]" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(test5, Greek, "xrh=n")
		try {
			val theseEntries:Option[Vector[PersEntry]] = pmj.entries	
		} catch {
			case t: Throwable => fail(s"Should have created an Option[Vector[PersEntry]].")
		}
	}

	it should "parse entries for a Greek verb" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(test5, Greek, "xrh=n")
		assert( pmj.entries.get.size == 5)
	}

	it should "parse entries when there is only one" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(oneEntry, Greek, "ou(=tos")
		assert( pmj.entries.get.size == 1)
	}

	it should "get a headword for a Greek verb" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(test5, Greek, "xrh=n")
		assert( pmj.entries.get(0).lexLemma == Some("χράω1"))
	}
	
	it should "get headwords for all entries" in {
		val pmj:PerseusMorphJson = PerseusMorphJson(test5, Greek, "xrh=n")
		val expected:Vector[String] = Vector("χράω1", "χράω2", "χραύω", "χρή", "χρῆ")
		assert( pmj.entries.get.map(_.lexLemma.get) == expected )
	}

	it should "return the Json for inflections" in {
		val doc:Json = parse(oneInfl).getOrElse(Json.Null)
		if (doc == Json.Null) assert(false)
		val obj:PerseusMorphJson = PerseusMorphJson(oneEntry, Greek, "ou(=tos")
		val il:List[Json] = obj.processInflJson(doc)
		assert( il(0) != Json.Null )
	}

	it should "return the Json for inflections when there are several" in {
		val doc:Json = parse(someInf).getOrElse(Json.Null)
		if (doc == Json.Null) assert(false)
		val obj:PerseusMorphJson = PerseusMorphJson(oneEntry, Greek, "ou(=tos")
		val il:List[Json] = obj.processInflJson(doc)
		assert( il.size == 3 )
		assert( il.contains(Json.Null) == false )
	}

	it should "return a PersEntry object for some JSON" in {
		val s:String = "οἱ"
		val lang:MorphLanguage = Greek
		val json:String = test1
		val pmj:PerseusMorphJson = PerseusMorphJson(json, lang, s)
	}

	it should "return a PersEntry object with one entry for some JSON" in {
		val s:String = "τῶν"
		val lang:MorphLanguage = Greek
		val json:String = test2
		val pmj:PerseusMorphJson = PerseusMorphJson(json, lang, s)
		assert(pmj.entries.get.size == 1)
	}

	it should "return a PersEntry object with one entry and three inflections for some JSON" in {
		val s:String = "τῶν"
		val lang:MorphLanguage = Greek
		val json:String = test2
		val pmj:PerseusMorphJson = PerseusMorphJson(json, lang, s)
		assert(pmj.entries.get.size == 1)
		assert(pmj.entries.get(0).inflections.size == 3)
	}

	"The perseusParser object" should "return JSON for a word in Greek" in {
		val jStr = PerseusParser.getMorphJson("καί", Greek)
		assert(jStr.contains(""""$": "conjunction""""))
	}
	it should "return JSON for a word in Latin" in {
		val jStr = PerseusParser.getMorphJson("et", Latin)
		assert(jStr.contains(""""$": "conjunction""""))
	}
	it should "return JSON for an elided word in Greek" in {
		val jStr = PerseusParser.getMorphJson("μυρίʼ", Greek)
		assert(jStr.contains("""μυρίος"""))
	}

	val test1:String = """{"RDF": {"Annotation": {"about": "urn:TuftsMorphologyService:oi(:morpheusgrc", "creator": {"Agent": {"about": "org.perseus:tools:morpheus.v1"}}, "created": {"$": "2019-03-20T14:53:20.812936"}, "hasTarget": {"Description": {"about": "urn:word:oi("}}, "title": {}, "hasBody": [{"resource": "urn:uuid:idm140550556017072"}, {"resource": "urn:uuid:idm140550555391472"}], "Body": [{"about": "urn:uuid:idm140550556017072", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "ἕ"}, "pofs": {"order": 5, "$": "pronoun"}}, "infl": [{"term": {"lang": "grc", "stem": {"$": "οἱ"}}, "pofs": {"order": 5, "$": "pronoun"}, "case": {"order": 5, "$": "dative"}, "gend": {"$": "masculine"}, "num": {"$": "singular"}, "dial": {"$": "epic Ionic"}, "stemtype": {"$": "pron3"}, "morph": {"$": "enclitic indeclform"}}, {"term": {"lang": "grc", "stem": {"$": "οἱ"}}, "pofs": {"order": 5, "$": "pronoun"}, "case": {"order": 5, "$": "dative"}, "gend": {"$": "feminine"}, "num": {"$": "singular"}, "dial": {"$": "epic Ionic"}, "stemtype": {"$": "pron3"}, "morph": {"$": "enclitic indeclform"}}]}}}, {"about": "urn:uuid:idm140550555391472", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "ὁ"}, "pofs": {"order": 0, "$": "article"}}, "infl": [{"term": {"lang": "grc", "stem": {"$": "οἱ"}}, "pofs": {"order": 0, "$": "article"}, "case": {"order": 7, "$": "nominative"}, "gend": {"$": "masculine"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "proclitic indeclform"}}, {"term": {"lang": "grc", "stem": {"$": "οἱ"}}, "pofs": {"order": 0, "$": "article"}, "case": {"order": 1, "$": "vocative"}, "gend": {"$": "masculine"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "proclitic indeclform"}}]}}}]}}}"""

	val test2:String = """{"RDF": {"Annotation": {"about": "urn:TuftsMorphologyService:tw=n:morpheusgrc", "creator": {"Agent": {"about": "org.perseus:tools:morpheus.v1"}}, "created": {"$": "2019-03-20T14:53:21.006266"}, "hasTarget": {"Description": {"about": "urn:word:tw=n"}}, "title": {}, "hasBody": {"resource": "urn:uuid:idm140550555259904"}, "Body": {"about": "urn:uuid:idm140550555259904", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "ὁ"}, "pofs": {"order": 0, "$": "article"}}, "infl": [{"term": {"lang": "grc", "stem": {"$": "τῶν"}}, "pofs": {"order": 0, "$": "article"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "feminine"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "indeclform"}}, {"term": {"lang": "grc", "stem": {"$": "τῶν"}}, "pofs": {"order": 0, "$": "article"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "masculine"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "indeclform"}}, {"term": {"lang": "grc", "stem": {"$": "τῶν"}}, "pofs": {"order": 0, "$": "article"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "neuter"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "indeclform"}}]}}}}}}"""

	val test3:String = """{"RDF": {"Annotation": {"about": "urn:TuftsMorphologyService:*dasku/lou:morpheusgrc", "creator": {"Agent": {"about": "org.perseus:tools:morpheus.v1"}}, "created": {"$": "2019-03-20T14:53:21.940477"}, "hasTarget": {"Description": {"about": "urn:word:*dasku/lou"}}, "title": {}, "hasBody": {"resource": "urn:uuid:idm140550555993248"}, "Body": {"about": "urn:uuid:idm140550555993248", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "Δάσκυλος"}, "pofs": {"order": 3, "$": "noun"}, "decl": {"$": "2nd"}, "gend": {"$": "masculine"}}, "infl": {"term": {"lang": "grc", "stem": {"$": "Δασκυλ"}, "suff": {"$": "ου"}}, "pofs": {"order": 3, "$": "noun"}, "decl": {"$": "2nd"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "masculine"}, "num": {"$": "singular"}, "stemtype": {"$": "os_ou"}}}}}}}}"""

	val test4:String = """{"RDF": {"Annotation": {"about": "urn:TuftsMorphologyService:u(perepaine/wn:morpheusgrc", "creator": {"Agent": {"about": "org.perseus:tools:morpheus.v1"}}, "created": {"$": "2019-03-20T14:53:27.362013"}, "hasTarget": {"Description": {"about": "urn:word:u(perepaine/wn"}}, "title": {}, "hasBody": {"resource": "urn:uuid:idm140550553759312"}, "Body": {"about": "urn:uuid:idm140550553759312", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "ὑπερεπαινέω"}, "pofs": {"order": 1, "$": "verb"}}, "infl": [{"term": {"lang": "grc", "stem": {"$": "ὑπερεπαιν"}, "suff": {"$": "έων"}}, "pofs": {"order": 0, "$": "verb participle"}, "case": {"order": 7, "$": "nominative"}, "gend": {"$": "masculine"}, "mood": {"$": "participle"}, "num": {"$": "singular"}, "tense": {"$": "present"}, "voice": {"$": "active"}, "dial": {"$": "epic Doric Ionic Aeolic"}, "stemtype": {"$": "ew_pr"}, "derivtype": {"$": "ew_denom"}}, {"term": {"lang": "grc", "stem": {"$": "ὑπέρ,ἐπί:αἰν"}, "suff": {"$": "έων"}}, "pofs": {"order": 0, "$": "verb participle"}, "case": {"order": 7, "$": "nominative"}, "gend": {"$": "masculine"}, "mood": {"$": "participle"}, "num": {"$": "singular"}, "tense": {"$": "present"}, "voice": {"$": "active"}, "dial": {"$": "epic Doric Ionic Aeolic"}, "stemtype": {"$": "ew_pr"}, "derivtype": {"$": "e_stem"}}]}}}}}}"""

	val test5:String = """{"RDF": {"Annotation": {"about": "urn:TuftsMorphologyService:xrh=n:morpheusgrc", "creator": {"Agent": {"about": "org.perseus:tools:morpheus.v1"}}, "created": {"$": "2019-03-20T14:53:29.222974"}, "hasTarget": {"Description": {"about": "urn:word:xrh=n"}}, "title": {}, "hasBody": [{"resource": "urn:uuid:idm140550556702880"}, {"resource": "urn:uuid:idm140550556519392"}, {"resource": "urn:uuid:idm140550554900880"}, {"resource": "urn:uuid:idm140550554832304"}, {"resource": "urn:uuid:idm140550554144080"} ], "Body": [{"about": "urn:uuid:idm140550556702880", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "χράω1"}, "pofs": {"order": 1, "$": "verb"} }, "infl": {"term": {"lang": "grc", "stem": {"$": "χρ"}, "suff": {"$": "ῆν"} }, "pofs": {"order": 1, "$": "verb"}, "mood": {"$": "infinitive"}, "tense": {"$": "present"}, "voice": {"$": "active"}, "dial": {"$": "Doric Ionic"}, "stemtype": {"$": "aw_pr"}, "derivtype": {"$": "a_stem"}, "morph": {"$": "contr"} } }} }, {"about": "urn:uuid:idm140550556519392", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "χράω2"}, "pofs": {"order": 1, "$": "verb"} }, "infl": [{"term": {"lang": "grc", "stem": {"$": "χρ"}, "suff": {"$": "ῆν"} }, "pofs": {"order": 1, "$": "verb"}, "mood": {"$": "infinitive"}, "tense": {"$": "present"}, "voice": {"$": "active"}, "dial": {"$": "Attic epic Doric"}, "stemtype": {"$": "ajw_pr"}, "derivtype": {"$": "a_stem"}, "morph": {"$": "contr"} }, {"term": {"lang": "grc", "stem": {"$": "χρ"}, "suff": {"$": "ῆν"} }, "pofs": {"order": 1, "$": "verb"}, "mood": {"$": "infinitive"}, "tense": {"$": "present"}, "voice": {"$": "active"}, "dial": {"$": "epic Doric Ionic"}, "stemtype": {"$": "ew_pr"}, "derivtype": {"$": "a_stem"}, "morph": {"$": "contr"} } ] }} }, {"about": "urn:uuid:idm140550554900880", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "χραύω"}, "pofs": {"order": 1, "$": "verb"} }, "infl": {"term": {"lang": "grc", "stem": {"$": "χρ"}, "suff": {"$": "ῆν"} }, "pofs": {"order": 1, "$": "verb"}, "mood": {"$": "infinitive"}, "tense": {"$": "present"}, "voice": {"$": "active"}, "dial": {"$": "Doric Ionic"}, "stemtype": {"$": "aw_pr"}, "derivtype": {"$": "av_stem"}, "morph": {"$": "contr"} } }} }, {"about": "urn:uuid:idm140550554832304", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "χρή"}, "pofs": {"order": 1, "$": "verb"} }, "infl": {"term": {"lang": "grc", "stem": {"$": "χρῆν"} }, "pofs": {"order": 1, "$": "verb"}, "mood": {"$": "indicative"}, "num": {"$": "singular"}, "pers": {"$": "3rd"}, "tense": {"$": "imperfect"}, "voice": {"$": "active"}, "stemtype": {"$": "ath_primary"} } }} }, {"about": "urn:uuid:idm140550554144080", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "χρῆ"}, "pofs": {"order": 3, "$": "noun"}, "decl": {"$": "1st"}, "gend": {"$": "feminine"} }, "infl": {"term": {"lang": "grc", "stem": {"$": "χρ"}, "suff": {"$": "ῆν"} }, "pofs": {"order": 3, "$": "noun"}, "decl": {"$": "1st"}, "case": {"order": 4, "$": "accusative"}, "gend": {"$": "feminine"}, "num": {"$": "singular"}, "dial": {"$": "epic Ionic"}, "stemtype": {"$": "eh_ehs"}, "morph": {"$": "contr"} } }} } ] }}}"""

val oneEntry:String = """{"RDF": {"Annotation": {"about": "urn:TuftsMorphologyService:οὗτος:morpheusgrc", "creator": {"Agent": {"about": "org.perseus:tools:morpheus.v1"}}, "created": {"$": "2019-03-22T05:25:21.772891"}, "hasTarget": {"Description": {"about": "urn:word:οὗτος"}}, "title": {}, "hasBody": {"resource": "urn:uuid:idm140649856448640"}, "Body": {"about": "urn:uuid:idm140649856448640", "type": {"resource": "cnt:ContentAsXML"}, "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "οὗτος"}, "pofs": {"order": 5, "$": "pronoun"}}, "infl": [{"term": {"lang": "grc", "stem": {"$": "οὗτος"}}, "pofs": {"order": 5, "$": "pronoun"}, "case": {"order": 7, "$": "nominative"}, "gend": {"$": "masculine"}, "num": {"$": "singular"}, "stemtype": {"$": "pron_adj1"}, "morph": {"$": "indeclform"}}, {"term": {"lang": "grc", "stem": {"$": "οὗτος"}}, "pofs": {"order": 5, "$": "pronoun"}, "case": {"order": 1, "$": "vocative"}, "gend": {"$": "masculine"}, "num": {"$": "singular"}, "stemtype": {"$": "pron_adj1"}, "morph": {"$": "indeclform"}}]}}}}}}"""

val oneInfl:String = """{"rest": {"entry": {"uri": "http://data.perseus.org/collections/urn:cite:perseus:latlexent.lex23489.1", "dict": {"hdwd": {"lang": "lat", "$": "ex"}, "pofs": {"order": 6, "$": "preposition"} }, "infl": {"term": {"lang": "lat", "stem": {"$": "ex"} }, "pofs": {"order": 6, "$": "preposition"}, "stemtype": {"$": "prep"}, "morph": {"$": "indeclform"} } }}}"""

val someInf:String = """ { "rest": {"entry": {"uri": null, "dict": {"hdwd": {"lang": "grc", "$": "ὁ"}, "pofs": {"order": 0, "$": "article"} }, "infl": [{"term": {"lang": "grc", "stem": {"$": "τῶν"} }, "pofs": {"order": 0, "$": "article"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "feminine"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "indeclform"} }, {"term": {"lang": "grc", "stem": {"$": "τῶν"} }, "pofs": {"order": 0, "$": "article"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "masculine"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "indeclform"} }, {"term": {"lang": "grc", "stem": {"$": "τῶν"} }, "pofs": {"order": 0, "$": "article"}, "case": {"order": 6, "$": "genitive"}, "gend": {"$": "neuter"}, "num": {"$": "plural"}, "stemtype": {"$": "article"}, "morph": {"$": "indeclform"} } ] }} }"""


}
