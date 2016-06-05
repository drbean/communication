module WordsCharacters where

import qualified Data.Map as Map

gfWords = Map.fromList [
	("A",a)
	, ("ADV",adv)
	, ("Aux",aux)
	, ("CONJ",conj)
	, ("Det",det)
	, ("N",n)
	, ("PN",pn)
	, ("Pron",pron)
	, ("Prep",prep)
	, ("Rel",rel)
	, ("Tag",tag)
	, ("V",v) ]

wordlist = concat ( map (gfWords Map.!) (Map.keys gfWords) )

posMap = Map.fromList [
	("A","Adjective")
	, ("ADV","Adverb")
	, ("Aux","Auxiliary")
	, ("CONJ","Conjunction")
	, ("Det","Determiner")
	, ("N","Noun")
	, ("PN","Proper Noun")
	, ("Pron","Pronoun")
	, ("Prep","Preposition")
	, ("Rel","Relative Pronoun")
	, ("Tag","Question Tag")
	, ("V","Verb")
	]

a = [

	"terrific"
	, "'Yes, but'"
	, "good at"
	, "inter-personal"
	, "lousy"
	, "non-judgmental"
	, "other"
	, "own"
	, "persuasive"
	, "second"

	]

adv = [

	"sometimes"
	, "up"
	, "very"
	, "well"
	, "so"
	, "always"
	, "for example"
	, "however"
	, "indeed"
	, "often"
	, "on the other hand"
	, "really"

	]

aux = [
	"doesn't"
	, "don't"
	, "does"
	, "do"
	, "is"
	, "are"
	, "isn't"
	, "aren't"
	, "would"
	, "should"
	]
	

conj = [

	"once"
	, "and"
	, "but"

	]


det = [
	"'s"
	, "some"
	, "no"
	, "0, _ or zero"
	, "a"
	, "an"
	, "no"
	, "the"

	]

n = [

	"student"
	, "tension between"
	, "understanding"
	, "way"
	, "some"
	, "someone"
	, "side"
	, "ability"
	, "accommodation mode"
	, "argument"
	, "assertion"
	, "assertiveness"
	, "capacity"
	, "concern"
	, "empathy"
	, "interest"
	, "kind"
	, "law student"
	, "listener"
	, "listening"
	, "need"
	, "negotiation"
	, "negotiation coach"
	, "one"
	, "perspective"
	, "problem"
	, "rebuttal"
	, "relation"
	, "repertoire"
	, "shoe"
	, "mind"
	, "person"
	, "people"

	]

pn = [

	"Mnookin"

	]

pron = [
	"who"
	, "their"
	, "them"
	, "themselves"
	, "they"
	, "this"
	, "we"
	, "when"
	, "where"
	, "your"
	, "both"
	, "that"
	, "what"
	, "which"
	, "you"
	, "how"
	, "she"
	, "her"
	, "he"
	, "his"
	]

prep = [

	"to"
	, "at"
	, "with"
	, "as"
	, "between"
	, "by"
	, "in"
	, "into"
	, "of"

	]

rel = [


	]

tag = [
	"doesn't he"
	, "doesn't she"
	, "doesn't it"
	, "don't they"
	, "does he"
	, "does she"
	, "does it"
	, "do they"
	, "isn't he"
	, "isn't she"
	, "isn't it"
	, "aren't they"
	, "is he"
	, "is she"
	, "is it"
	, "are they"
	]

v = [

	, "articulate"
	"stand"
	, "understand"
	, "call"
	, "can"
	, "compose"
	, "defend"
	, "describe"
	, "do"
	, "expand"
	, "fall"
	, "have"
	, "help"
	, "learn"
	, "listen"
	, "lose"
	, "make"
	, "managing"
	, "may"
	, "mean"
	, "put"
	, "relate"

	]

{-

ability	: CN;
accommodation mode	: N;
always	: Adv;
and	: Conj;
argument	: CN;
articulate	: V2;
as	: Prep;
assertion	: N;
assertiveness	: N;
at	: Prep;
between	: Prep;
both	: Pron;
but	: Conj;
by	: Prep;
call	: V3;
can	: VV;
capacity	: CN;
compose	: V2;
concern	: CN;
defend	: V2;
describe	: V2;
do	: V2;
empathy	: N;
expand	: V2;
fall	: V2;
for example	: Adv;
good at	: A;
have	: VV;
help	: V2V;
how
however	: Adv;
in	: Prep;
indeed	: Adv;
interest	: CN;
inter-personal	: A;
into	: Prep;
kind	: N2;
law student	: CN;
learn	: V2;
listen	: V2;
listener	: CN;
listening	: N;
lose	: V2;
lousy	: A;
making	: V2;
managing	: V2;
may	: VV;
mean	: V2;
mind	: CN;
Mnookin	: PN;
need	: CN;
negotiation	: N;
negotiation coach	: CN;
non-judgmental	: A;
of	: Prep;
often	: Adv;
on the other hand	: Adv;
once	: Subj;
one	: N;
other	: A;
own	: A;
person
perspective	: CN;
persuasive	: A;
problem	: N2;
put	: V3;
really	: Adv;
rebuttal	: CN;
relate	: V2;
relation	: CN;
repertoire	: CN;
second	: A;
shoe	: CN;
side	: CN;
so	: Adv;
some	: N;
someone	: N;
sometimes	: Adv;
stand	: V;
student	: N2;
tension between	: N2;
terrific	: A;
that
the
their	: Pron;
them	: Pron;
themselves	: Pron;
they	: Pron;
this	: Pron;
to	: Prep;
understand	: V2;
understanding	: N2;
up	: Adv;
very	: Adv;
way	: CN;
we	: Pron;
well	: Adv;
what
when	: Pron;
where	: Pron;
with	: Prep;
'Yes, but'	: A;
your	: Pron;


-}

-- vim: set ts=2 sts=2 sw=2 noet:
