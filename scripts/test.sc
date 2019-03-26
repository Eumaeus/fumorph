
fumm.distinct.map( mf => {

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