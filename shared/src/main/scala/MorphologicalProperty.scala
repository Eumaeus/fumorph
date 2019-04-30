package edu.furman.classics.fumorph

sealed trait MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

/* Case */

sealed trait GrammaticalCase extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Nominative extends GrammaticalCase {
	val long:String = "nominative"
	val abbr:String = "nom."
	val short:String = "n"
	override def toString = long
}
case object Genitive extends GrammaticalCase {
	val long:String = "genitive"
	val abbr:String = "gen."
	val short:String = "g"
	override def toString = long
}
case object Dative extends GrammaticalCase {
	val long:String = "dative"
	val abbr:String = "dat."
	val short:String = "d"
	override def toString = long
}
case object Accusative extends GrammaticalCase {
	val long:String = "accusative"
	val abbr:String = "acc."
	val short:String = "a"
	override def toString = long
}
case object Vocative extends GrammaticalCase {
	val long:String = "vocative"
	val abbr:String = "voc."
	val short:String = "v"
	override def toString = long
}
case object Locative extends GrammaticalCase {
	val long:String = "locative"
	val abbr:String = "loc."
	val short:String = "l"
	override def toString = long
}
case object Ablative extends GrammaticalCase {
	val long:String = "ablative"
	val abbr:String = "abl."
	val short:String = "b"
	override def toString = long
}

/* Gender */

sealed trait Gender extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Masculine extends Gender {
	val long:String = "masculine"
	val abbr:String = "masc."
	val short:String = "m"
	override def toString = long
}
case object Feminine extends Gender {
	val long:String = "feminine"
	val abbr:String = "fem."
	val short:String = "f"
	override def toString = long
}
case object Neuter extends Gender {
	val long:String = "neuter"
	val abbr:String = "neut."
	val short:String = "n"
	override def toString = long
}
case object MascFemNeuter extends Gender {
	val long:String = "masculine, feminine, neuter"
	val abbr:String = "masc.,fem.,neut."
	val short:String = "mfn"
	override def toString = long
}
/* Number */

sealed trait GrammaticalNumber extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Singular extends GrammaticalNumber {
	val long:String = "singular"
	val abbr:String = "sing."
	val short:String = "s"
	override def toString = long
}

case object Dual extends GrammaticalNumber {
	val long:String = "dual"
	val abbr:String = "dl."
	val short:String = "d"
	override def toString = long
}

case object Plural extends GrammaticalNumber {
	val long:String = "plural"
	val abbr:String = "pl."
	val short:String = "p"
	override def toString = long
}

/* Degree */

sealed trait Degree extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Positive extends Degree {
	val long:String = "positive"
	val abbr:String = "pos."
	val short:String = "p"
	override def toString = long
}
case object Comparative extends Degree {
	val long:String = "comparative"
	val abbr:String = "comp."
	val short:String = "c"
	override def toString = long
}
case object Superlative extends Degree {
	val long:String = "superlative"
	val abbr:String = "sup."
	val short:String = "s"
	override def toString = long
}

/* Person */

sealed trait Person extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object First extends Person {
	val long:String = "1st person"
	val abbr:String = "1st"
	val short:String = "1"
	override def toString = long
}
case object Second extends Person {
	val long:String = "2nd person"
	val abbr:String = "2nd"
	val short:String = "2"
	override def toString = long
}
case object Third extends Person {
	val long:String = "3rd person"
	val abbr:String = "3rd"
	val short:String = "3"
	override def toString = long
}

/* Tense */

sealed trait Tense extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Present extends Tense {
	val long:String = "present"
	val abbr:String = "pres."
	val short:String = "p"
	override def toString = long
}
case object Imperfect extends Tense {
	val long:String = "imperfect"
	val abbr:String = "impf."
	val short:String = "i"
	override def toString = long
}
case object Future extends Tense {
	val long:String = "future"
	val abbr:String = "fut."
	val short:String = "f"
	override def toString = long
}
case object Aorist extends Tense {
	val long:String = "aorist"
	val abbr:String = "aor."
	val short:String = "a"
	override def toString = long
}
case object Perfect extends Tense {
	val long:String = "perfect"
	val abbr:String = "perf."
	val short:String = "r"
	override def toString = long
}
case object Pluperfect extends Tense {
	val long:String = "pluperfect"
	val abbr:String = "plpf."
	val short:String = "l"
	override def toString = long
}
case object FuturePerfect extends Tense {
	val long:String = "future perfect"
	val abbr:String = "futperf."
	val short:String = "t"
	override def toString = long
}

/* Mood */

sealed trait Mood extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}
case object Indicative extends Mood {
	val long:String = "indicative"
	val abbr:String = "indic."
	val short:String = "i"
	override def toString = long
}
case object Subjunctive extends Mood {
	val long:String = "subjunctive"
	val abbr:String = "subj."
	val short:String = "s"
	override def toString = long
}
case object Optative extends Mood {
	val long:String = "optative"
	val abbr:String = "opt."
	val short:String = "o"
	override def toString = long
}
case object Imperative extends Mood {
	val long:String = "imperative"
	val abbr:String = "imper."
	val short:String = "m"
	override def toString = long
}

/* Voice */

sealed trait Voice extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Active extends Voice {
	val long:String = "active"
	val abbr:String = "act."
	val short:String = "a"
	override def toString = long
}
case object Middle extends Voice {
	val long:String = "middle"
	val abbr:String = "mid."
	val short:String = "m"
	override def toString = long
}
case object Passive extends Voice {
	val long:String = "passive"
	val abbr:String = "pass."
	val short:String = "p"
	override def toString = long
}
case object MedioPassive extends Voice {
	val long:String = "medio-passive"
	val abbr:String = "mid-pass."
	val short:String = "e"
	override def toString = long
}
case object Deponent extends Voice {
	val long:String = "deponent"
	val abbr:String = "depon."
	val short:String = "o"
	override def toString = long
}

/* Indeclinables */

sealed trait IndeclinablePoS extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
	override def toString = long
}

case object Conjunction extends IndeclinablePoS {
	val long:String = "conjunction"
	val abbr:String = "conj."
	val short:String = "c"
	override def toString = long
}
case object Particle extends IndeclinablePoS {
	val long:String = "particle"
	val abbr:String = "particle"
	val short:String = "c"
	override def toString = long
}
case object Preposition extends IndeclinablePoS {
	val long:String = "preposition"
	val abbr:String = "prep."
	val short:String = "r"
	override def toString = long
}
case object Interjection extends IndeclinablePoS {
	val long:String = "interjection"
	val abbr:String = "interj."
	val short:String = "i"
	override def toString = long
}

case object Unmatched extends IndeclinablePoS {
	val long:String = "unmatched"
	val abbr:String = "unmatched"
	val short:String = "x"
	override def toString = long
}

