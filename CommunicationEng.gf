--# -path=.:./engine:/home/drbean/GF/lib/src/translator:present

concrete CommunicationEng of Communication = MyConcrete  **
open ConstructorsEng, ParadigmsEng, StructuralEng, IrregEng, ExtraEng, ConstructX, Prelude, (R=ResEng) in {

-- oper

lin

-- Adv

	really	= mkAdv "really";
	up	= mkAdv "up";
	very	= mkAdv "very";
	well	= mkAdv "well";
	sometimes	= mkAdv "sometimes";
	so	= mkAdv "so";
	always	= mkAdv "always";
	for_example	= mkAdv "for example";
	however	= mkAdv "however";
	indeed	= mkAdv "indeed";
	often	= mkAdv "often";
	on_the_other_hand	= mkAdv "on the other hand";

-- AP

	terrific	= mkA2( mkA "terrific") at;
	yes_but	= mkAP( mkA "'Yes, but'") ;
	good_at	= mkA2( mkA "good") at;
	inter_personal	= mkAP( mkA "inter-personal") ;
	lousy	= mkAP( mkA "lousy") ;
	non_judgmental	= mkAP( mkA "non-judgmental") ;
	other	= mkAP( mkA "other") ;
	own	= mkAP( mkA "own") ;
	persuasive	= mkAP( mkA "persuasive") ;
	second	= mkAP( mkA "second") ;

-- Conj

	and	= mkConj "and";
	but	= mkConj "but";

-- Det


-- N

	student	= mkN2( mkN "student") ;
	tension_between	= mkN2( mkN "tension") between;
	understanding	= mkN2( mkN "understanding") ;
	way	= mkCN( mkN "way") ;
	side	= mkCN( mkN "side") ;
	someone	= mkN "someone" nonExist;
	ability	= mkN2( mkN "ability") to;
	accommodation_mode	= mkN "accommodation mode" nonExist;
	argument	= mkCN( mkN "argument") ;
	assertion	= mkN "assertion" nonExist;
	assertiveness	= mkN "assertiveness" nonExist;
	capacity	= mkN2( mkN "capacity") to;
	concern	= mkCN( mkN "concern") ;
	empathy	= mkN "empathy" nonExist;
	interest	= mkCN( mkN "interest") ;
	kind	= mkN2( mkN "kind") ;
	law_student	= mkCN( mkN "law student") ;
	listener	= mkCN( mkN "listener") ;
	listening	= mkN "listening" nonExist;
	mind	= mkCN( mkN "mind") ;
	need	= mkCN( mkN "need") ;
	negotiation	= mkN "negotiation" nonExist;
	negotiation_coach	= mkCN( mkN "negotiation coach") ;
	one	= mkN "one" nonExist;
	perspective	= mkCN( mkN "perspective") ;
	problem	= mkN2( mkN "problem") ;
	rebuttal's	= mkCN( mkN "rebuttal's") ;
	relation	= mkCN( mkN "relation") ;
	repertoire	= mkCN( mkN "repertoire") ;
	shoe	= mkCN( mkN "shoe") ;

-- PN

	mnookin	= mkPN( mkN feminine (mkN "Mnookin") );

-- Prep

	between	= mkPrep "between";
	at	= mkPrep "at";
	to	= mkPrep "to";
	with_prep	= mkPrep "with";
	as	= mkPrep "as";
	by	= mkPrep "by";
	in_prep	= mkPrep "in";
	in_manner	= mkPrep "in";
	in_loc	= mkPrep "in";
	into	= mkPrep "into";

-- Pron

	both	= mkNP (mkDet (mkQuant "one" "both") plNum);

-- Subj

	once	= mkSubj "once";

-- V

	articulate	= mkV2( mkV "articulate") noPrep;
	stand	= partV( mkV "stand") "";
	understand	= mkV2( mkV "understand") noPrep;
	call	= mkV3( mkV "call") ;
	compose	= mkV2( mkV "compose") noPrep;
	defend	= mkV2( mkV "defend") noPrep;
	describe	= mkV2( mkV "describe") noPrep;
	do	= mkV2( mkV "do") noPrep;
	expand	= mkV2( mkV "expand") noPrep;
	fall	= mkV2( mkV "fall") into;
	have_to	= mkVV( mkV "have") ;
	help	= mkV2V( mkV "help") noPrep noPrep;
	learn	= mkV2( mkV "learn") noPrep;
	listen	= mkV2( mkV "listen") noPrep;
	lose	= mkV2( mkV "lose") noPrep;
	make	= mkV2( mkV "make" "made") noPrep;
	manage	= mkV2( mkV "manage") noPrep;
	may	= mkVV( mkV "may") ;
	mean	= mkV2( mkV "mean") noPrep;
	put	= mkV3 (mkV "put") noPrep in_prep;
	relate	= mkV2( mkV "relate") to;

}

-- vim: set ts=2 sts=2 sw=2 noet:
