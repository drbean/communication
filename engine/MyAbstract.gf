abstract MyAbstract = Cat, Conjunction ** {


	flags startcat = Utt ;

cat
	Time;
	Times;
	TimeName;
	Period;
	Title;
	Place;
	PlaceNoun;
	LocPrep;
	Motion;
	CoagentPrep;
	InstrumentPrep;
	ThemePrep;
	MannerPrep;
	TimePrep;
	SourcePrep;
	ResultPrep;
	PatientPrep;
	Adv_coagent;
	Adv_instrument;
	Adv_theme;
	Adv_manner;
	Adv_time;
	Adv_location;
	Adv_source;
	Adv_result;
	Adv_patient;
	MassDet;
	SubordCl;
	Partitive;
	NounCl;

fun
	Look_bad	: VA -> AP -> VP;
	-- Be_made_sth : V3 -> NP -> VP;
	Be_bad	: AP -> Comp;
	Be_someone	: NP -> Comp;
	Be_AdV_NP	: AdV -> NP -> Comp;
	Be_somewhere	: Adv_location -> Comp;
	Be_coagent	: Adv_coagent -> Comp;
	Be_vp	: Comp -> VP;
	Locating  : LocPrep -> Place -> Adv_location;
	Location	: Det -> PlaceNoun -> Place;
	NamedPlace	: PN -> Place;
	FreqAdv	: NP -> Period -> Time;
	PeriodAdv	: Times -> Period;
	Coagency	: CoagentPrep -> NP -> Adv_coagent;
	Instrumenting	: InstrumentPrep -> NP -> Adv_instrument;
	Themeing	: ThemePrep -> NP -> Adv_theme;
	Mannering	: MannerPrep -> NP -> Adv_manner;
	Timing		: TimePrep -> NP -> Adv_time;
	Sourcing		: SourcePrep -> NP -> Adv_source;
	Resulting		: ResultPrep -> NP -> Adv_result;
	Patienting		: PatientPrep -> NP -> Adv_patient;
	Happening	: V -> VP ;
	Changing	: V2 -> NP -> VP;
	V_NP_VP:	V2V -> NP -> VP -> VP;
	Intens:	VV -> VP -> VP;
	V_that_S:	VS -> S -> VP;
	V_S:	VS -> S -> VP;
	V_SC:	VS -> SC -> VP;
	V_NP_that_S:	V2S -> NP -> S -> VP;
	V_NP_S:	V2S -> NP -> S -> VP;
	V_NP_whether_S:	V2Q -> NP -> QS -> VP;
	V_NP_NP:	V3 -> NP -> NP -> VP;
  V_NP_AP: V2A -> NP -> AP -> VP;
	GetPassV3	: V3 -> NP -> VP ;	-- get called John
	-- GetNPPPart	: V2 -> NP -> VP; -- get the job done right
	passive : V2 -> VP;
	Pass : VPSlash -> VP;
	PassAgent : VPSlash -> NP -> VP;
	V2Slash	: V2 -> VPSlash;
	-- VSSlash	: VS -> VPSlash;
	V2VSlash	: V2V -> VP -> VPSlash;
	V2ASlash	: V2A -> AP -> VPSlash;
	V3Slash	: V3 -> NP -> VPSlash;
	reflexive	: VPSlash -> VP;
	ModInf : CN -> VP -> CN;
	ModPass3 : CN -> V3 -> NP -> CN;
	-- ModSlInf : CN -> VPSlash -> CN;
	MassModInf : N -> VP -> CN;
	Modified	: CN -> RCl -> CN;
	MassMod	: N -> RCl -> N;
	SubjRel	: RP -> VP -> RCl;
	ObjRel	: RP -> ClSlash -> RCl;
	EmptyRel : ClSlash -> RCl;
	EmptyRelSlash	: ClSlash -> RCl;
	WayNP	: Cl -> NP;
	HowNP	: Cl -> NP;
	ThatNP	: Cl -> NP;
	PartN	: V -> N;
	Gerund	: VP -> NP;
	GerundSlash	: VPSlash -> CN;
	ByGerund : VP -> Adv_manner;
	SClSlash	: NP -> VPSlash -> ClSlash;
	-- VPClSlash	: VPSlash -> ClSlash;
	FreeICl : IP -> VP -> NounCl;
	FreeIClSlash : IP -> ClSlash -> NounCl;
	FreeInfICl	: IAdv -> VP -> NounCl;
	-- FreeInfCl	: VP -> NounCl;
	NomCl : NounCl -> NP;
	Mannered	: NP -> Adv_manner -> NP;
	Sourced	: NP -> Adv_source -> NP;
	Themed	: NP -> Adv_theme -> NP;
	AdV_VP	: AdV -> VP -> VP;
	AdV_VPSlash	: AdV -> VPSlash -> VPSlash;
	WithPlace	:  V -> Adv_location -> VP;
	WithTime	: Time -> VP -> VP;
	VP_Adv_coagent	: VP -> Adv_coagent -> VP;
	VP_Adv_instrument	: VP -> Adv_instrument -> VP;
	VP_Adv_theme	: VP -> Adv_theme -> VP;
	VP_Adv_manner : VP -> Adv_manner -> VP;
	VP_Adv_time	: VP -> Adv_time -> VP;
	VP_Adv_location	:  Motion -> Adv_location -> VP;
	VP_Adv_result	: VP -> Adv_result -> VP;
	WithCl	: VP -> SubordCl -> VP;
	VPToo	: VP -> VP;
	VPAlready	: VP -> VP;
	WithClPre	: SubordCl -> S -> S;
	WithAdvPre	: Adv -> S -> S;
	ThemePre	: Adv_theme -> S -> S;
	PatientPre	: Adv_patient -> S -> S;

	ICompS	: IComp -> NP -> QS;
	YN	: Cl -> QCl;

	TagQ	: NP -> VP -> QCl;
	TagComp	: NP -> Comp -> QCl;
	TagModal	: NP -> VV -> VP -> QCl;
	-- TagNP	: NP -> NP -> QCl;
	-- TagAP	: NP -> AP -> QCl;

	WH_Pred	: IP -> VP -> QCl;
	WHose	: CN -> IP;
	IPhrase	: IDet -> CN -> IP;
	WH_ClSlash	: IP -> ClSlash -> QCl;
	PosQ	: QCl -> QS;
	NegQ	: QCl -> QS;
	PosS	: Cl -> S;
	NegS	: Cl -> S;
	QUt	: QS -> Utt;
	Ut	: S -> Utt;
	Sentence	: NP -> VP -> Cl;
	Exist	: NP -> Cl;

	Yes, No, NoAnswer	: Utt;
	Answer : NP -> Utt;

	Inject	: Interj -> SC;

	Entity	: PN -> NP;
	Kind	: AP -> CN -> CN;
	MassKind	: AP -> N -> N;
	KindOfKind  : CN -> Adv -> CN;
	KindInPlace	: CN -> Adv_location -> CN;
	NPInPlace	: NP -> Adv_location -> NP;
	PlaceKind	: AP -> PlaceNoun -> PlaceNoun;
	Membership : Det -> CN -> Adv_location -> Cl;
	CompoundCN	: CN -> CN -> CN;
	Item	: Det -> CN -> NP;
	MassItem	: MassDet -> N	-> NP;
	Titular	: Title -> NP;
	PredetItem	: Predet -> NP -> NP;
	Ofpos	: N2 -> NP -> CN;
	Ofpart	: Partitive -> N -> CN;
	N2toCN	: N2 -> CN;
	ApposNP	: NP -> NP -> NP;
	NPPostPredet	: NP -> Predet -> NP;

	a_DET : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	zero_Det_pl : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	zero_Det_sg : MassDet;
	the_MASS_DET	: MassDet;
	some_MASS_DET	: MassDet;
	any_MASS_DET	: MassDet;
	the_SG_DET : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	the_PLURAL_DET : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	Apos  : NP -> Det;
	MassApos	: NP -> MassDet;
	Apos_pl  : NP -> Det;
	no_DET	: Det;
	no_PL_DET	: Det;
	no_NP	: NP;
	no_PL_NP	: NP;
	no_MASSDET	: MassDet;
	some_DET	: Det;
	some_PL_DET	: Det;
	some_NP	: NP;
	some_PL_NP	: NP;
	some_PREDET	: Predet;
	List : NP -> NP -> ListNP;
	AddList : NP -> ListNP -> ListNP;
	CloseList	: Conj -> ListNP -> NP;
	APList : AP -> AP -> ListAP;
	AddAP : AP -> ListAP -> ListAP;
	CloseAP	: Conj -> ListAP -> AP;
	ConcatS	: Conj -> S -> S -> S;
	PreConjUtt	: Conj -> Utt -> Utt;

	her_DET	: Det;
	her_MASSDET	: MassDet;
	his_DET	: Det;
	its	: Det;
	your	: Det;
	their	: Det;
	this	: Det;

	he	: NP;
	she	: NP;
	it	: NP;
	that_PRON	: NP;
	this_PRON	: NP;
	they	: NP;
	you	: NP;
	we	: NP;

	who_WH	: IP;
	what_WH	: IP;
	what_PL_IDET	: IDet;
	which_SG_IDET	: IDet;
	how_WH	: IAdv;
	who_RP	: RP;
	that_RP	: RP;
	in_which	: RP;
	where_RP	: RP;
	when_RP	: RP;

	more : CAdv;
	ComparaAP : A -> NP -> AP;
	ComparaAdv : CAdv -> A -> NP -> Adv;
	ComparaS : AP -> S -> AP;
	More	: A -> AP;
	AdjModified	: AP -> VP -> AP;
	As_as	: AP -> NP -> AP;
	AdvAdj	: AdA -> AP -> AP;
	A_PP	: A2 -> NP ->AP;
	VP_AP	: VP -> AP;
	VP_NP_AP	: VPSlash -> NP -> AP;

	about_PREP	: Prep;
	at_PREP	: LocPrep;
	before_PREP	: Prep;
	from_PREP	: Prep;
  like_PREP	: Prep;
	of_PREP	: Prep;
	part_prep	: Prep;
	up_PREP	: Prep;

	person	: CN;
	thing	: CN;
	entity	: CN;

	can	: VV;
	have	: V2;
	know_V2	: V2;
	know_VS	: VS;

	Not_Adv	: Adv -> Adv;
	Very_Adv	: Adv -> Adv;
	In_order_to : VP -> Adv;
	To_purpose	: VP -> Adv;
	because_SUBJ	: Subj;
	if_SUBJ	: Subj;
	when_SUBJ	: Subj;
	so_SUBJ	: Subj;
	or_CONJ	: Conj;

	Subjunct	: Subj -> S -> SubordCl;

}

-- vim: set ts=2 sts=2 sw=2 noet:
