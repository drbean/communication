module Representation (module Representation, module Communication) where

import Data.DRS
import Data.DRS.Show

import Communication
import PGF

import Model
import Interpretation
-- import Story_Interpretation
-- import qualified Topic_Interpretation as Topic

import Data.Char
import Data.List
import Data.Tuple

entuples :: [(Entity,GPN)]
entuples = [
	]

ref2int :: DRSRef -> Int
ref2int (DRSRef r@('r':[d])) | isDigit d , n <- digitToInt d = n
ref2int r = error ("No digit for DRSRef " ++ drsRefToDRSVar r)

isDummy :: DRSRef -> Bool
isDummy (DRSRef ('d':'u':'m':'m':'y':[d])) | isDigit d = True
isDummy _ = False

int2ref :: Int -> DRSRef
int2ref n = DRSRef ("r" ++ show n )

lc_first :: String -> String
lc_first str@(s:ss) = if any (flip isPrefixOf str) [
	"Mnookin"
	 ]
	then s:ss
	else toLower s:ss

instance Eq GPN where
	(==) _ _ = False

gent2ent :: GPN -> Entity
gent2ent gent        | Just ent <- lookup gent (map swap entuples) = ent
ent2gent :: Entity -> GPN
ent2gent ent | Just gent <- lookup ent entuples = gent

lin :: Gf a => a -> String
lin e = stripApp (unApp (gf e))

stripApp :: Maybe (CId, [Expr]) -> String
stripApp = maybe "Undefined" (showCId . fst)

linNP :: GNP -> String
linNP (GEntity name) = lin name
linNP (GItem _ (GKind _ (GOfpos noun _))) = lin noun
linNP (GItem _ (GKind _ noun)) = lin noun
linNP (GItem _ noun) = lin noun
linNP (GMassItem _ noun) = lin noun
linNP (GCloseList _ (GList np1 np2)) = linNP np1 ++ "_or_" ++ linNP np2

linAP :: GAP -> String
linAP (GAdvAdj _ a) = lin a
linAP (GCloseAP _ (GAPList ap1 ap2)) = linAP ap1 ++ "_or_" ++ linAP ap2
linAP a = lin a

linIP :: GIP -> String
linIP Gwho_WH = "person"
linIP Gwhat_WH = "thing"

-- e2t :: GPN -> Tree
-- e2t e | (Just tr) <- unApp (gf e) = tr

unmaybe (Just x) = x
-- unmaybe Nothing = I

repS :: GUtt -> Maybe (DRSRef -> DRS)

repS (GQUt (GPosQ (GYN (GSentence np (GVP_Adv_instrument (GPass (GV2ASlash v ap))
	(GInstrumenting _ arg)))))) =
	repS (GQUt (GPosQ (GYN (GSentence arg (GV_NP_AP v np ap)))))
repS (GQUt (GPosQ (GYN (GSentence np (GVP_Adv_instrument (GPass vp) pp))))) =
	repS (GQUt (GPosQ (GYN (GSentence np (GPass vp)))))
repS (GQUt (GPosQ (GYN (GSentence np (GPass (GV2ASlash v ap)))))) =
	repS (GQUt (GPosQ (GYN (GSentence (GItem Ga_Det Gentity) (GV_NP_AP v np ap)))))

repS (GQUt (GPosQ (GYN (GSentence np vp)))) = Just (repNP np (repVP vp))
repS (GQUt (GNegQ (GYN (GSentence np vp)))) =
	repS (GQUt (GPosQ (GYN (GSentence np vp))))
repS (GQUt (GPosQ (GYN (GMembership det cn (GLocating _ np))))) =
	Just (repPlace np (repVP (GChanging Ghave (GItem det cn))))
repS (GQUt (GPosQ (GTagComp np comp))) =
	repS (GQUt (GPosQ (GYN (GSentence np (GBe_vp comp)))))
repS (GQUt (GPosQ (GTagQ np vp))) = repS (GQUt (GPosQ (GYN (GSentence np vp))))
repS (GQUt (GPosQ (GWH_Pred wh vp))) = Just (repW wh (repVP vp))

new :: GNP -> [DRSRef] -> DRSRef
new Gshe (r:_) = r
new Ghe (r:_) = r
new np rs = let
	es = rs
	max = ref2int (maximum rs)
	r':rs' = newDRSRefs [int2ref max] es in r'

newOnPart :: GPartitive -> [DRSRef] -> DRSRef
newOnPart _ rs = new (GItem Ga_Det Gperson) rs

newOnPos :: GN2 -> [DRSRef] -> DRSRef
newOnPos _ rs = new (GItem Ga_Det Gperson) rs

newOnPlace :: GPlace -> [DRSRef] -> DRSRef
newOnPlace _ rs = new (GItem Ga_Det Gperson) rs

repNP :: GNP -> (DRSRef -> DRS) -> DRSRef -> DRS
repNP (GItem det cn) p r = repDet det (repCN cn) p r
repNP (GMassItem det n) p r = (repMassDet det) (repN n) p r
repNP (GEntity name) p r
	| entity <- (gent2ent name) , entity `elem` entities = let
	DRS rs conds = p r
	len = length (nub rs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) [] in
	DRS reflist (Rel (DRSRel (lin name)) [r] : conds)
repNP (GCloseList or_Conj nps@(GList np1 np2)) p r = p r
repNP Gshe p r = let
	dummy =DRSRef "dummy1"
	i = ref2int r
	iminus = ref2int r - 1
	rolled_ref = int2ref iminus
	she_refs = case r of
		(DRSRef "r1") -> [DRSRef "r1"]
		_ -> newDRSRefs (replicate i (DRSRef "r")) []
	DRS rs conds = p dummy
	reals = filter (not . isDummy) rs
	len = case reals of
		[] -> 1
		_ -> ref2int (maximum reals)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	she_conds = foldl1 (\cs1 cs2 -> [Or
		(DRS [] cs1 )
		(DRS [] cs2)]) (map (sheDRS conds dummy ) she_refs) in
	(DRS reflist she_conds) where
	sheDRS :: [DRSCon] -> DRSRef -> DRSRef -> [DRSCon]
	sheDRS conds d r = (female r: (map (subst d r) conds))
	female :: DRSRef -> DRSCon
	female r = (Rel (DRSRel "female") [r])
	subst :: DRSRef -> DRSRef -> DRSCon -> DRSCon
	subst d r c = case c of
		(Rel rel rs) -> Rel rel (
			map (\x -> if x == d then r else x) rs
			)
		(Neg (DRS reflist conds)) ->
			Neg (DRS reflist (map (subst d r) conds))
		(Prop ref (DRS reflist conds)) ->
			Prop ref (DRS reflist (map (subst d r) conds))
		_	-> c
repNP Ghe p r = let
	dummy =DRSRef "dummy1"
	i = ref2int r
	iminus = ref2int r - 1
	rolled_ref = int2ref iminus
	he_refs = case r of
		(DRSRef "r1") -> [DRSRef "r1"]
		_ -> newDRSRefs (replicate i (DRSRef "r")) []
	DRS rs conds = p dummy
	reals = filter (not . isDummy) rs
	len = case reals of
		[] -> 1
		_ -> ref2int (maximum reals)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	he_conds = foldl1 (\cs1 cs2 -> [Or
		(DRS [] cs1 )
		(DRS [] cs2)]) (map (heDRS conds dummy ) he_refs) in
	(DRS reflist he_conds) where
	heDRS :: [DRSCon] -> DRSRef -> DRSRef -> [DRSCon]
	heDRS conds d r = (male r: (map (subst d r) conds))
	male :: DRSRef -> DRSCon
	male r = (Rel (DRSRel "male") [r])
	subst :: DRSRef -> DRSRef -> DRSCon -> DRSCon
	subst d r c = case c of
		(Rel rel rs) -> Rel rel (
			map (\x -> if x == d then r else x) rs
			)
		(Neg (DRS reflist conds)) ->
			Neg (DRS reflist (map (subst d r) conds))
		(Prop ref (DRS reflist conds)) ->
			Prop ref (DRS reflist (map (subst d r) conds))
		_	-> c



repDet :: GDet -> (DRSRef -> DRS) -> (DRSRef -> DRS) -> DRSRef -> DRS
repDet Ga_Det = \ p q r-> let
	DRS prs pconds = p r
	DRS qrs qconds = q (maximum prs)
	len = ref2int (maximum qrs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	conds = pconds ++ qconds
	in DRS reflist conds
repDet Gno_Det = repDet Ga_Det
repDet Gher_Det = \ p q dummy-> let
	iminus = ref2int dummy - 1
	rolled_ref = int2ref iminus
	her_refs = case dummy of
		(DRSRef "r1") -> [DRSRef "r1"]
		_ -> newDRSRefs (replicate iminus (DRSRef "r")) []
	DRS prs pconds = p dummy
	thing = maximum prs
	DRS qrs qconds = q thing
	len = ref2int (maximum qrs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	her_cond = foldl (\ors dummy -> Or
		(DRS [] [ female dummy, have dummy thing] )
		(DRS [] [ors])) false her_refs
	conds = pconds ++ [her_cond] ++ qconds
	in DRS reflist conds where
	female :: DRSRef -> DRSCon
	female r = (Rel (DRSRel "female") [r])
	have :: DRSRef -> DRSRef -> DRSCon
	have r thing = Rel (DRSRel "have") [r, thing]
	false :: DRSCon
	false = Rel (DRSRel "true") [DRSRef "r1"]
repDet Ghe_Det = \ p q dummy-> let
	iminus = ref2int dummy - 1
	rolled_ref = int2ref iminus
	he_refs = case dummy of
		(DRSRef "r1") -> [DRSRef "r1"]
		_ -> newDRSRefs (replicate iminus (DRSRef "r")) []
	DRS prs pconds = p dummy
	thing = maximum prs
	DRS qrs qconds = q thing
	len = ref2int (maximum qrs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	he_cond = foldl (\ors dummy -> Or
		(DRS [] [ male dummy, have dummy thing] )
		(DRS [] [ors])) false he_refs
	conds = pconds ++ [he_cond] ++ qconds
	in DRS reflist conds where
	male :: DRSRef -> DRSCon
	male r = (Rel (DRSRel "male") [r])
	have :: DRSRef -> DRSRef -> DRSCon
	have r thing = Rel (DRSRel "have") [r, thing]
	false :: DRSCon
	false = Rel (DRSRel "true") [DRSRef "r1"]
repDet Gsome_Det = repDet Ga_Det
repDet GtheSg_Det = repDet Ga_Det
repDet Gsome_pl_Det = repDet Gsome_Det
repDet Gzero_Det_pl = repDet Gsome_pl_Det
repDet GthePlural_Det =  repDet Gsome_pl_Det
repDet (GApos np) = case np of
	Gshe -> repDet Gher_Det
	Ghe -> repDet Ghe_Det
	_ -> \p q r -> let
		len = ref2int r
		owner = r
		thing = int2ref (len + 1)
		reflist = newDRSRefs (replicate len (DRSRef "r")) []
		ownership =  Rel (DRSRel "have") [owner, thing]
		DRS prs pconds = p thing
		DRS qrs qconds = q (maximum prs)
		len' = length (nub (r: prs ++ qrs))
		reflist' = newDRSRefs (replicate len' (DRSRef "r")) []
		conds = pconds ++ [ownership] ++ qconds
		in repNP np (\ ref -> DRS reflist' conds ) owner
repDet (GApos_pl owner) = repDet (GApos owner)

repMassDet :: GMassDet -> (DRSRef -> DRS) -> (DRSRef -> DRS) -> DRSRef -> DRS
repMassDet Gzero_Det_sg = \ p q r-> let
	DRS prs pconds = p r
	DRS qrs qconds = q (maximum prs)
	len = ref2int (maximum (prs ++ qrs))
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	conds = pconds ++ qconds
	in DRS reflist conds
repMassDet Gthe_mass_Det = repMassDet Gzero_Det_sg
repMassDet (GMassApos owner) = \p q r -> let
	len = ref2int r
	iminus = len - 1
	iplus = len + 1
	(owner_ref,thing) = case owner of
		Gshe -> (int2ref iminus, int2ref len)
		Ghe -> (int2ref iminus, int2ref len)
		_ -> (int2ref len, int2ref iplus)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	ownership_cond =  Rel (DRSRel "have") [owner_ref, thing]
	DRS prs pconds = p thing
	DRS qrs qconds = q (maximum prs)
	len' = length (nub (r: prs ++ qrs))
	reflist' = newDRSRefs (replicate len' (DRSRef "r")) []
	conds = pconds ++ [ownership_cond] ++ qconds
	in repNP owner (\ ref -> DRS reflist' conds ) owner_ref
repMassDet her_MassDet = \ p q r-> let
	her_refs = newDRSRefs (replicate (ref2int r - 1) (DRSRef "r")) []
	DRS prs pconds = p r
	thing = maximum prs
	DRS qrs qconds = q thing
	len = ref2int (maximum qrs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) []
	her_cond = foldl (\ors r -> Or
		(DRS [] [ female r, have r thing] )
		(DRS [] [ors])) false her_refs
	conds = pconds ++ [her_cond] ++ qconds
	in DRS reflist conds where
	female :: DRSRef -> DRSCon
	female r = (Rel (DRSRel "female") [r])
	have :: DRSRef -> DRSRef -> DRSCon
	have r thing = Rel (DRSRel "have") [r, thing]
	false :: DRSCon
	false = Rel (DRSRel "true") [DRSRef "r1"]

repN :: GN -> DRSRef -> DRS
repN name = \r ->
	DRS [r] [Rel (DRSRel (lin name)) [r]]
repN2 :: GN2 -> DRSRef -> DRS
repN2 name     = \r ->
	DRS [r] [Rel (DRSRel (lin name)) [r]]
repPartitive :: GPartitive -> DRSRef -> DRS
repPartitive name     = \r ->
	DRS [r] [Rel (DRSRel (lin name)) [r]]

repCN :: GCN -> DRSRef -> DRS
repCN (GKind ap cn) = \r -> let
       DRS thing_refs thing_conds = (repCN cn r)
       DRS attri_refs attri_conds = (repAP ap r)
       in DRS (thing_refs ++ attri_refs) (thing_conds ++ attri_conds)
repCN (GOfpart part n) = \r -> let
	form = r
	thing = newOnPart part [r]
	DRS _ partconds = repPartitive part form
	DRS _ thingconds = repN n thing
	formcond = [ Rel (DRSRel "in_form_of") [thing,form] ]
	in DRS [form, thing] (partconds ++ thingconds ++ formcond)
repCN (GOfpos n2 np) = \r -> let
	owner = r
	thing = new np [r]
	DRS rs conds = repN2 n2 thing in
	repNP np (\owner -> let
	newconds = conds ++ [Rel (DRSRel "have") [owner, thing]]
	in DRS [owner, thing, newOnPos n2 [thing]] newconds ) owner
repCN (GModified cn rs) = \r -> let
	DRS attri_refs attri_conds = case rs of
		(GSubjRel wh vp) -> repVP vp r
	DRS thing_refs thing_conds = repCN cn r
	reflist = nub (attri_refs ++ thing_refs) in
	DRS reflist (attri_conds ++ thing_conds)
repCN name     = \r ->
	DRS [r] [Rel (DRSRel (lin name)) [r]]

repAP :: GAP -> DRSRef -> DRS
repAP (GAdvAdj _ a) = \ r -> DRS [r] [Rel (DRSRel (lin a)) [r]]
repAP (GCloseAP _ (GAPList ap1 ap2)) = \r -> DRS [r] [
	Rel (DRSRel ((linAP ap1) ++ "_or_" ++ (linAP ap2))) [r] ]
repAP ap = \r -> DRS [r] [Rel (DRSRel (linAP ap)) [r]]

repPlace :: GPlace -> (DRSRef -> DRS) -> DRSRef -> DRS
repPlace (GLocation det name) p r = (repDet det) (repPlaceNoun name) p r
repPlace place p r = let
	DRS rs conds = p r
	len = ref2int (maximum rs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) [] in
	(DRS reflist ((Rel (DRSRel (lin place)) [r]) : conds))

repPlaceNoun :: GPlaceNoun -> DRSRef -> DRS
repPlaceNoun (GPlaceKind ap name) = \r -> let
       DRS name_refs name_conds = (repPlaceNoun name r)
       DRS attri_refs attri_conds = (repAP ap r)
       in DRS (name_refs ++ attri_refs) (name_conds ++ attri_conds)
repPlaceNoun name = \r -> DRS [r] [Rel (DRSRel (lin name)) [r]]

repVP :: GVP -> DRSRef -> DRS
repVP (GWithCl vp _) = repVP vp
repVP (GWithTime _ vp) = repVP vp
repVP (GBe_vp comp) = case comp of
	(GBe_someone np) -> \r -> let
		DRS rs conds = repNP np
			(\ hypernym -> DRS [hypernym] [] ) r
		len = ref2int (maximum rs)
		reflist = newDRSRefs (replicate len (DRSRef "r")) [] in
		DRS rs conds
	GBe_bad ap -> repAP ap
	GBe_somewhere (GLocating prep place) -> \r -> 
		repPlace place (\name -> DRS [r,name]
		[ Rel (DRSRel (lin prep)) [r, name]]
		) (newOnPlace place [r])
repVP (GWithPlace v (GLocating prep place)) = \r ->
	repPlace place (\name -> DRS [r,name]
	[ Rel (DRSRel (lin v)) [r,name]]
	) (newOnPlace place [r])
repVP (GVP_Adv_location v (GLocating prep place)) = \r ->
	repPlace place (\name -> DRS [r,name]
	[ Rel (DRSRel (lin v)) [r,name]]
	) (newOnPlace place [r])
repVP (GVP_Adv_coagent v (GCoagency prep np)) = \r ->
	repNP np (\style -> DRS [r,style]
	[ Rel (DRSRel (lin v)) [r,style]]
	) (new np [r])
repVP (GVP_Adv_instrument vp (GInstrumenting prep np)) = repVP vp
repVP (GLook_bad v ap) = \r -> let
	patient = r
	DRS rs' [Rel rel rs] = repAP ap patient
	lin_ap = linAP ap
	p = DRSRef "p"
	look_conds = [Rel (DRSRel (lin v)) [patient, p]
		, Prop p (DRS [] [Rel (DRSRel lin_ap) rs])]
	in DRS [patient] look_conds
repVP (GHappening v) = \r -> DRS [r] [Rel (DRSRel (lin v)) [r]]
repVP (GChanging v obj) = \r -> repNP obj
	(\patient -> DRS [r,patient] [Rel (DRSRel (lin v)) [r, patient]] ) (new obj [r])
repVP (GV_NP_NP v obj1 obj2) = \r -> repNP obj1 (\theme ->
		repNP obj2 (\recipient ->
			DRS [r,theme,recipient] [Rel (DRSRel (lin v)) [r, theme, recipient]]
			) (new obj2 [r,theme]) ) (new obj1 [r])
repVP (GV_NP_AP v obj ap) = \r -> repNP obj (\patient -> 
		DRS [r,patient] [Rel (DRSRel (lin v)) [r, patient]
			, Rel (DRSRel (linAP ap)) [patient]]) (new obj [r])
repVP (GV_that_S v0 (GPosS (GSentence np vp))) = case vp of
	(GBe_vp comp) -> case comp of
		(GBe_bad ap ) -> \r -> repNP np (\referent -> let
			d = repAP ap referent
			p = DRSRef "p" in
			DRS [referent] [Rel (DRSRel (lin v0)) [r,p]
				, Prop p d] ) (new np [r])
		(GBe_someone subjcomp ) -> \r -> repNP np (\referent -> let
			p = DRSRef "p"
			cond = [Rel (DRSRel (lin v0)) [r, p]
				, Prop p (DRS []
				[Rel (DRSRel (linNP subjcomp)) [referent] ])]
			in DRS [referent] cond ) r
		(GBe_somewhere (GLocating prep place)) -> \r -> repNP np (\referent -> let
			lin_v = lin v0
			p = DRSRef "p"
			conds = [Rel (DRSRel lin_v) [r,p]
				, Prop p (DRS [] [Rel
				(DRSRel (lin place)) [referent] ])]
			in DRS [r,referent] conds )
			(new np [r])
	(GChanging v obj) -> \r ->
			repNP np (\referent -> repNP obj (\theme -> let
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin v0)) [r,p]
				, Prop p (DRS [] [Rel
				(DRSRel lin_v) [referent,theme]])]
			in DRS [r,referent,theme] conds
			) (new obj [r,referent]) ) (new np [r])
	(GVP_Adv_manner vp2 _) -> repVP (GV_that_S v0 (GPosS (GSentence np vp2)))
	(GIntens vv vp2) -> case vp2 of
		(GChanging v obj) -> \r ->
			repNP np (\referent -> repNP obj (\theme -> let
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin vv)) [r, p]
				, Prop p (DRS [] [Rel 
				(DRSRel lin_v) [referent, theme]])]
			in DRS [r, theme, referent] conds )
			(new obj [r,referent]) ) (new np [r])
		(GV_NP_NP v obj1 obj2) -> \r ->
			repNP np (\referent -> repNP obj1 (\theme ->
			repNP obj2 (\recipient -> let
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin vv)) [r, p]
				, Prop p (DRS [] [Rel 
				(DRSRel lin_v) [referent, theme, recipient]])]
			in DRS [r, referent, theme, recipient] conds
			) (new obj2 [r,referent,theme]) ) (new obj1 [r,referent]) ) (new np [r])
	(GPass (GV2Slash v) ) -> \r -> repNP np (\patient -> (\agent ->
		DRS [r,patient,agent] [Rel (DRSRel (lin v0)) [r, DRSRef "p"]
		, Prop (DRSRef "p") (DRS [] [Rel (DRSRel (lin v)) [agent, patient]] ) ])
		(new (GItem Ga_Det Gperson) [r,patient]) ) (new np [r])
repVP (GV_S v0 (GPosS (GSentence np vp))) = 
	repVP (GV_that_S v0 (GPosS (GSentence np vp)))
repVP (GV_that_S v0 (GNegS (GSentence np vp))) = case vp of
	(GVP_Adv_manner vp2 _) -> repVP (GV_that_S v0 (GNegS (GSentence np vp2)))
	(GIntens vv vp2) -> case vp2 of
		(GChanging v obj) -> \r -> repNP np (\referent ->
			repNP obj (\theme -> let
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin v0)) [r, p]
				, Prop p (DRS [] [Neg (DRS []
				[Rel (DRSRel lin_v)
				[referent, theme]])])]
			in DRS [r, theme, referent] conds )
			(new obj [r,referent]) ) (new np [r])
		(GVP_Adv_manner vp3 _) ->
			repVP (GV_that_S v0 (GNegS (GSentence np (GIntens vv vp3))))
		(GHappening v) -> \r -> repNP np (\referent -> let
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin v0)) [r,p]
				, Prop p (DRS [] [Neg (DRS []
				[Rel (DRSRel lin_v) [referent]])])]
			in DRS [r,referent] conds ) (new np [r])
		(GV_NP_VP v obj vp3) -> case vp3 of
			(GVP_Adv_manner vp4 _) ->
				repVP (GV_that_S v0 (GNegS (GSentence np (GIntens vv
				(GV_NP_VP v obj vp4)))))
			(GHappening v1) -> \r -> repNP np (\referent ->
				repNP obj (\theme -> let
					statement = DRSRel (lin v0)
					intensive = DRSRel (lin v)
					intransitive = DRSRel (lin v1)
					p1 = DRSRef "p1"
					p2 = DRSRef "p2"
					p3 = DRSRef "p3"
					conds = [Rel statement [r,p1]
						, Prop p1 (DRS [] [Neg (DRS [] [Prop p2 (DRS []
						[Rel intensive [referent,p2], (Prop p3 (DRS []
						[Rel intransitive [referent]]))])])])]
					in DRS [r,theme,referent] conds )
					(new obj [r,referent]) ) (new np [r])
repVP (GV_S v0 (GNegS (GSentence np vp))) = 
	repVP (GV_that_S v0 (GNegS (GSentence np vp)))
repVP (GV_NP_whether_S v0 np0 (GPosQ (GYN (GSentence np vp)))) = case vp of
	(GBe_vp comp) -> case comp of
		(GBe_bad ap) -> \r -> repNP np0 (\recipient -> 
			repNP np (\referent -> let 
			d = repAP ap referent
			lin_v = lin v0
			p = DRSRef "p"
			conds = [Rel (DRSRel lin_v) [r,recipient,p]
				, Prop p d]
			in DRS [r,recipient,referent] conds
				) (new np [r,recipient]) ) (new np0 [r])
		(GBe_someone subjcomp ) -> \r -> repNP np0 (\recipient ->
			repNP np (\referent -> let
			lin_v = lin v0
			p = DRSRef "p"
			conds = [Rel (DRSRel lin_v) [r, recipient, p]
				, Prop p (DRS [] [Rel 
				(DRSRel (linNP subjcomp)) [referent] ])]
			in DRS [r,recipient,referent] conds )
			(new np [r,recipient]) ) (new np0 [r])
		(GBe_somewhere (GLocating prep place)) -> \r -> repNP np0 (\recipient ->
			repNP np (\referent -> let
			lin_v = lin v0
			p = DRSRef "p"
			conds = [Rel (DRSRel lin_v) [r,recipient,p]
				, Prop p (DRS [] [Rel
				(DRSRel (lin place)) [referent] ])]
			in DRS [r,recipient,referent] conds )
			(new np [r,recipient]) ) (new np0 [r])
repVP (GV_NP_whether_S v0 np0 (GICompS how_old np)) =
	\r -> repNP np0 (\recipient -> repNP np (\referent -> let
		lin_v = lin v0
		p = DRSRef "p"
		conds = [Rel (DRSRel lin_v) [r,recipient,p]
			, Prop p (DRS [] [Rel
			(DRSRel "how_old") [referent] ])]
		in DRS [r,recipient,referent] conds )
	(new np [r,recipient]) ) (new np0 [r])
repVP (GV_NP_that_S v0 np0 (GPosS (GSentence np vp))) = case vp of
	(GBe_vp comp) -> case comp of
		(GBe_bad ap ) -> \r -> repNP np0 (\recipient -> 
			repNP np (\referent -> let
			d = repAP ap referent
			p = DRSRef "p" in
			DRS [referent] [Rel (DRSRel (lin v0)) [r, recipient, p]
				, Prop p d] ) (new np [r,recipient]) ) (new np [r])
		(GBe_someone subjcomp ) -> \r -> repNP np0 (\recipient ->
			repNP np (\referent -> let
			lin_v = lin v0
			p = DRSRef "p"
			conds = [Rel (DRSRel lin_v) [r,recipient,p]
				, Prop p (DRS [] [Rel
				(DRSRel (linNP subjcomp)) [referent] ])]
			in DRS [r,recipient,referent] conds )
			(new np [r,recipient]) ) (new np0 [r])
		(GBe_somewhere (GLocating prep place)) -> \r -> repNP np0 (\recipient ->
			repNP np (\referent -> let
			lin_v = lin v0
			p = DRSRef "p"
			conds = [Rel (DRSRel lin_v) [r,recipient,p]
				, Prop p (DRS [] [Rel
				(DRSRel (lin place)) [referent] ])]
			in DRS [r,recipient,referent] conds )
			(new np [r,recipient]) ) (new np0 [r])
repVP (GV_NP_S v0 np0 (GPosS (GSentence np vp))) =
	repVP (GV_NP_that_S v0 np0 (GPosS (GSentence np vp)))
repVP (GV_NP_VP v0 obj vp) = case vp of
	(GLook_bad v ap) -> \r ->
		repNP obj (\patient -> let
			lin_v0 = lin v0
			lin_v = lin v
			DRS rs' [Rel rel rs] = repAP ap patient
			lin_ap = linAP ap
			p1 = DRSRef "p1"
			p2 = DRSRef "p2"
			conds = [Rel (DRSRel lin_v0) [r, p1]
				, Prop p1 (DRS [] [Rel
				(DRSRel lin_v) [patient, p2]
					, Prop p2 (DRS [] [Rel (DRSRel lin_ap) rs])])]
			in DRS [r, patient] conds ) (new obj [r])
	(GV_NP_VP v1 obj1 vp1) -> \r ->
		repNP obj (\recipient -> case vp1 of
			(GChanging v2 obj2) -> repNP obj1 (\theme ->
				repNP obj2 (\goal -> let
				lin_v0 = lin v0
				lin_v1 = lin v1
				lin_v2 = lin v2
				conds = [Rel (DRSRel lin_v0) [r, recipient]
					, Rel (DRSRel lin_v1) [recipient, theme]
					, Rel (DRSRel lin_v2) [theme, goal] ]
				in DRS [r, recipient, theme, goal] conds )
				(new obj2 [r,recipient,theme]) ) (new obj1 [r,recipient]) ) (new obj [r])
repVP (GIntens v0 vp) = case vp of
	(GVP_Adv_coagent v (GCoagency prep np)) -> \r ->
		repNP np (\coagent -> let
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin v0)) [r, p]
				, Prop p (DRS [] [Rel (DRSRel lin_v)
				[r, coagent]]) ]
			in DRS [r,coagent] conds ) (new np [r])
	(GWithTime _ v) -> repVP (GIntens v0 v)
	(GVP_Adv_location v (GLocating prep place)) -> \r ->
		repPlace place (\name -> let 
			lin_v = lin v
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin v0)) [r, p]
				, Prop p (DRS [] [Rel (DRSRel lin_v)
				[r, name]] ) ]
			in DRS [r,name] conds) (newOnPlace place [r])
	(GBe_vp comp) -> case comp of
		(GBe_someone np) -> \r ->
			repNP np (\ hypernym -> let
			p = DRSRef "p"
			conds = [Rel (DRSRel (lin v0)) [r, p]
				, Prop p (DRS [] [Rel (DRSRel "be")
				[r, hypernym]])]
			in DRS [hypernym] conds) (new np [r])
	(GChanging v obj) -> \r ->
		repNP obj (\theme -> let
		lin_v = lin v
		p = DRSRef "p"
		conds = [Rel (DRSRel (lin v0)) [r, p]
			, Prop p (DRS [] [Rel (DRSRel lin_v)
			[r, theme]])]
		in DRS [theme] conds) (new obj [r])
	(GV_NP_NP v obj1 obj2) -> \r ->
		repNP obj1 (\theme ->
		repNP obj2 (\recipient -> let
		lin_v = lin v
		p = DRSRef "p"
		conds = [Rel (DRSRel (lin v0)) [r, p]
			, Prop p (DRS [] [Rel (DRSRel lin_v) [r, theme, recipient]])]
		in DRS [r,theme,recipient] conds)
			(new obj2 [r,theme]) ) (new obj1 [r])
repW :: GIP -> (DRSRef -> DRS) -> DRSRef -> DRS
repW Gwho_WH p r = let
	person = Rel (DRSRel "person") [r]
	DRS rs conds = p r
	len = ref2int (maximum rs)
	reflist = newDRSRefs (replicate len (DRSRef "r")) [] in
	DRS reflist ( person : conds)
repW (GWHose cn) p r = let
	owned = new (GItem Ga_Det cn) [r]
	ownership_conds =  [ Rel (DRSRel "have") [r, owned] ]
	DRS rs conds = repCN cn owned
	DRS prs pconds = p owned
	len = ref2int (maximum (rs ++ prs))
	reflist = newDRSRefs (replicate len (DRSRef "r")) [] in
	DRS reflist (conds ++ ownership_conds ++ pconds)

-- repW Gwhat_WH p = Merge (DRS [x] [Rel (DRSRel "thing") [x] ] ) (p x)

-- vim: set ts=2 sts=2 sw=2 noet:
