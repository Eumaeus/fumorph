package edu.furman.classics.fumorph

sealed trait MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

/* Case */

sealed trait GrammaticalCase extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Nominative extends GrammaticalCase {
	val long:String = "nominative"
	val abbr:String = "nom."
	val short:String = "n"
}
case object Genitive extends GrammaticalCase {
	val long:String = "genitive"
	val abbr:String = "gen."
	val short:String = "g"
}
case object Dative extends GrammaticalCase {
	val long:String = "dative"
	val abbr:String = "dat."
	val short:String = "d"
}
case object Accusative extends GrammaticalCase {
	val long:String = "accusative"
	val abbr:String = "acc."
	val short:String = "a"
}
case object Vocative extends GrammaticalCase {
	val long:String = "vocative"
	val abbr:String = "voc."
	val short:String = "v"
}
case object Locative extends GrammaticalCase {
	val long:String = "locative"
	val abbr:String = "loc."
	val short:String = "l"
}
case object Ablative extends GrammaticalCase {
	val long:String = "ablative"
	val abbr:String = "abl."
	val short:String = "b"
}

/* Gender */

sealed trait Gender extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Masculine extends Gender {
	val long:String = "masculine"
	val abbr:String = "masc."
	val short:String = "m"
}
case object Feminine extends Gender {
	val long:String = "feminine"
	val abbr:String = "fem."
	val short:String = "f"
}
case object Neuter extends Gender {
	val long:String = "neuter"
	val abbr:String = "neut."
	val short:String = "n"
}

/* Number */

sealed trait GrammaticalNumber extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Singular extends GrammaticalNumber {
	val long:String = "singular"
	val abbr:String = "sing."
	val short:String = "s"
}

case object Dual extends GrammaticalNumber {
	val long:String = "dual"
	val abbr:String = "dl."
	val short:String = "d"
}

case object Plural extends GrammaticalNumber {
	val long:String = "plural"
	val abbr:String = "pl."
	val short:String = "p"
}

/* Degree */

sealed trait Degree extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Positive extends Degree {
	val long:String = "positive"
	val abbr:String = "pos."
	val short:String = "p"
}
case object Comparative extends Degree {
	val long:String = "comparative"
	val abbr:String = "comp."
	val short:String = "c"
}
case object Superlative extends Degree {
	val long:String = "superlative"
	val abbr:String = "sup."
	val short:String = "s"
}

/* Person */

sealed trait Person extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object First extends Person {
	val long:String = "1st person"
	val abbr:String = "1st"
	val short:String = "1"
}
case object Second extends Person {
	val long:String = "2nd person"
	val abbr:String = "2nd"
	val short:String = "2"
}
case object Third extends Person {
	val long:String = "3rd person"
	val abbr:String = "3rd"
	val short:String = "3"
}

/* Tense */

sealed trait Tense extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Present extends Tense {
	val long:String = "present"
	val abbr:String = "pres."
	val short:String = "p"
}
case object Imperfect extends Tense {
	val long:String = "imperfect"
	val abbr:String = "impf."
	val short:String = "i"
}
case object Future extends Tense {
	val long:String = "future"
	val abbr:String = "fut."
	val short:String = "f"
}
case object Aorist extends Tense {
	val long:String = "aorist"
	val abbr:String = "aor."
	val short:String = "a"
}
case object Perfect extends Tense {
	val long:String = "perfect"
	val abbr:String = "perf."
	val short:String = "r"
}
case object Pluperfect extends Tense {
	val long:String = "pluperfect"
	val abbr:String = "plpf."
	val short:String = "l"
}
case object FuturePerfect extends Tense {
	val long:String = "future perfect"
	val abbr:String = "futperf."
	val short:String = "t"
}

/* Mood */

sealed trait Mood extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}
case object Indicative extends Mood {
	val long:String = "indicative"
	val abbr:String = "indic."
	val short:String = "i"
}
case object Subjunctive extends Mood {
	val long:String = "subjunctive"
	val abbr:String = "subj."
	val short:String = "s"
}
case object Optative extends Mood {
	val long:String = "optative"
	val abbr:String = "opt."
	val short:String = "o"
}
case object Imperative extends Mood {
	val long:String = "imperative"
	val abbr:String = "imper."
	val short:String = "m"
}

/* Voice */

sealed trait Voice extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Active extends Voice {
	val long:String = "active"
	val abbr:String = "act."
	val short:String = "a"
}
case object Middle extends Voice {
	val long:String = "middle"
	val abbr:String = "mid."
	val short:String = "m"
}
case object Passive extends Voice {
	val long:String = "passive"
	val abbr:String = "pass."
	val short:String = "p"
}
case object MedioPassive extends Voice {
	val long:String = "medio-passive"
	val abbr:String = "mid-pass."
	val short:String = "e"
}
case object Deponent extends Voice {
	val long:String = "deponent"
	val abbr:String = "depon."
	val short:String = "o"
}

/* Indeclinables */

sealed trait IndeclinablePoS extends MorphologicalProperty {
	val long:String
	val abbr:String 
	val short:String
}

case object Conjunction extends IndeclinablePoS {
	val long:String = "conjunction"
	val abbr:String = "conj."
	val short:String = "c"
}
case object Particle extends IndeclinablePoS {
	val long:String = "particle"
	val abbr:String = "particle"
	val short:String = "c"
}
