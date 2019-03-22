package edu.furman.classics.fumorph

sealed trait Form

object Form {
  /*
  def apply(s: String): Form = {
  }
  */
}


case class VerbForm(person: Person, grammaticalNumber: GrammaticalNumber, tense: Tense, mood: Mood, voice: Voice) extends Form {}

case class IndeclinableForm(pos: IndeclinablePoS) extends Form {}

case class NounForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber) extends Form {}

case class AdjectiveForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree) extends Form {}

case class PronounForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree) extends Form {}

case class ArticleForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber, degree: Degree) extends Form {}

case class AdverbForm(degree: Degree) extends Form {}

case class InfinitiveForm(tense:Tense, voice:Voice) extends Form {}

case class ParticipleForm(tense:Tense, voice:Voice, gender:Gender, grammaticalCase:GrammaticalCase, grammaticalNumber:GrammaticalNumber) extends Form {}

case class VerbalAdjectiveForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber) extends Form {}

case class GerundForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber) extends Form {}

case class GerundiveForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber) extends Form {}

case class SupineForm(gender: Gender, grammaticalCase: GrammaticalCase, grammaticalNumber: GrammaticalNumber) extends Form {}
