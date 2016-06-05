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
	FreeRCl : IP -> VP -> NounCl;
	FreeRClSlash : IP -> ClSlash -> NounCl;
	FreeInfCl	: IAdv -> VP -> NounCl;
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

	a_Det : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	zero_Det_pl : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	zero_Det_sg : MassDet;
	the_mass_Det	: MassDet;
	some_mass_Det	: MassDet;
	any_mass_Det	: MassDet;
	theSg_Det : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	thePlural_Det : Det; -- (\d,f -> exists (\x -> and (d x) (f x)));
	Apos  : NP -> Det;
	MassApos	: NP -> MassDet;
	Apos_pl  : NP -> Det;
	no_Det	: Det;
	no_pl_Det	: Det;
	no_NP	: NP;
	no_pl_NP	: NP;
	no_MassDet	: MassDet;
	some_Det	: Det;
	some_pl_Det	: Det;
	some_NP	: NP;
	some_pl_NP	: NP;
	some_Predet	: Predet;
	List : NP -> NP -> ListNP;
	AddList : NP -> ListNP -> ListNP;
	CloseList	: Conj -> ListNP -> NP;
	APList : AP -> AP -> ListAP;
	AddAP : AP -> ListAP -> ListAP;
	CloseAP	: Conj -> ListAP -> AP;
	ConcatS	: Conj -> S -> S -> S;
	PreConjUtt	: Conj -> Utt -> Utt;

	her_Det	: Det;
	her_MassDet	: MassDet;
	he_Det	: Det;
	its	: Det;
	your	: Det;
	their	: Det;
	this	: Det;

	he	: NP;
	she	: NP;
	it	: NP;
	that_Pron	: NP;
	this_Pron	: NP;
	they	: NP;
	you	: NP;
	we	: NP;

	who_WH	: IP;
	what_WH	: IP;
	whatPl_IDet	: IDet;
	whichSg_IDet	: IDet;
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

	about_prep	: Prep;
	at_prep	: LocPrep;
	before_prep	: Prep;
	from_prep	: Prep;
  like_prep	: Prep;
	of_prep	: Prep;
	part_prep	: Prep;
	up_prep	: Prep;

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
	because_Subj	: Subj;
	if_Subj	: Subj;
	when_Subj	: Subj;
	so_Subj	: Subj;
	or_Conj	: Conj;

	Subjunct	: Subj -> S -> SubordCl;

}

-- vim: set ts=2 sts=2 sw=2 noet:
