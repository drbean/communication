module WordsCharacters where

import Data.Char

import PGF
import System.Environment.FindBin

-- path = getProgPath
-- file = path >>= \p -> return ( (++) p "/Happier.pgf")
-- gr = file >>= \f -> return ( readPGF f )
gr = readPGF "/home/drbean/GF/gf-contrib/drbean/business/conflict/communication/Communication.pgf"

cat2funs :: String -> IO [CId]
cat2funs cat = do
	gr' <- gr
	let fs = functionsByCat gr' (mkCId cat)
	let ws = filter (isLower . head . showCId) fs
	let is = map (reverse . dropWhile (\x ->  (==) x '_' || isUpper x) . reverse .showCId ) ws
	return (map mkCId is )

gfWords :: [(String, IO [CId])]
gfWords = [
	("A",a)
	, ("Adv",adv)
	-- , ("Aux",aux)
	, ("Conj",conj)
	, ("Det",det)
	, ("N",n)
	, ("CN",cn)
	, ("PN",pn)
	-- , ("Pron",pron)
	, ("Prep",prep)
	-- , ("Rel",rel)
	, ("Tag",tag)
	, ("V",v)
	, ("V2",v2)
	, ("V3",v3)
	, ("VV",vv)
	, ("VS",vs)
	, ("V2A",v2a)
	]

posName :: String -> String
posName "A"	= "Adjective"
posName "Adv"	= "Adverb"
posName "Aux"	= "Auxiliary"
posName "Conj"	= "Conjunction"
posName "Det"	= "Determiner"
posName "N"	= "Uncount Noun"
posName "CN"	= "Count Noun"
posName "PN"	= "Proper Noun"
posName "Pron"	= "Pronoun"
posName "Prep"	= "Preposition"
posName "Rel"	= "Relative Pronoun"
posName "Tag"	= "Question Tag"
posName "V"	= "Verb"
posName "V2"	= "Verb + object"
posName "V3"	= "Verb + obj1 + obj2"
posName "VV"	= "Verb + verb"
posName "VS"	= "Verb + sentence"
posName "V2S"	= "Verb + object + sentence"
posName "V2A"	= "Verb + object + adjective"


a	= cat2funs "AP"
adv	= cat2funs "Adv"
conj	= cat2funs "Conj"
det	= cat2funs "Det"
n	= cat2funs "N"
cn	= cat2funs "CN"
pn	= cat2funs "PN"
prep	= cat2funs "Prep"
v	= cat2funs "V"
v2	= cat2funs "V2"
v3	= cat2funs "V3"
vv	= cat2funs "VV"
vs	= cat2funs "VS"
v2a	= cat2funs "V2A"
tag = return ( map mkCId tags )





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

rel = [


	]

tags = [
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
