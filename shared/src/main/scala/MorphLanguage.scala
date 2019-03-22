package edu.furman.classics.fumorph



sealed trait MorphLanguage {
	val abbr:String
}

object Latin extends MorphLanguage {
	val abbr:String = "lat"
}
object Greek extends MorphLanguage {
	val abbr:String = "grc"
}
