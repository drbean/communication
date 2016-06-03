module Model where 

import Data.Tuple
import Data.List
import Data.Maybe
data Entity	= A | B | C | D | E | F | G
  | H | I | J | K | L | M | N 
  | O | P | Q | R | S | T | U 
  | V | W | X | Y | Z | Someone | Something | Unspec
     deriving (Eq,Show,Bounded,Enum,Ord)

entities :: [Entity]
entities	=  [minBound..maxBound] 

entity_check :: [ (Entity, String) ]
entity_check =  [
  (A, "" )
  , (B, "" )
  , (C, "" )
  , (D, "Doctor" )
  , (E, "mercury_theater" )
  , (F, "" )
  , (G, "" )
  , (H, "" )
  , (I, "" )
  , (J, "joe" )
  , (K, "" )
  , (L, "" )
  , (M, "mars" )
  , (N, "new_jersey" )
  , (O, "" )
  , (P, "" )
  , (Q, "" )
  , (R, "" )
  , (S, "" )
  , (T, "the_doctor" )
  , (U, "" )
  , (V, "" )
  , (W, "orson_welles" )
  , (X, "" )
  , (Y, "" )
  , (Z, "" )
  ]

ent_ided :: String -> Entity
ent_ided name = head [entity | (entity,string) <- entity_check ,
				name == string
				]

characters :: [(String,Entity)]
characters = findEnt entity_check
	where findEnt e_maps = 
		[(name,e) | (e,name) <- e_maps
			, name /= ""]

stringEntity :: [(String,Entity)]
stringEntity = map swap entity_check

namelist :: [String]
namelist = [string | (entity,string) <- entity_check, string /= "" ]

predid1 :: String -> Maybe OnePlacePred
predid2 :: String -> Maybe TwoPlacePred
predid3 :: String -> Maybe ThreePlacePred
predid4 :: String -> Maybe FourPlacePred

predid3 name
       | Just pred <- lookup name threePlacers = Just pred
        -- | otherwise    = Nothing
        | otherwise    = error $ "no '" ++ name ++ "' three-place predicate."
predid4 name
       | Just pred <- lookup name fourPlacers = Just pred
        -- | otherwise    = Nothing
        | otherwise    = error $ "no '" ++ name ++ "' four-place predicate."
predid5 name
       | Just pred <- lookup name fivePlacers = Just pred
        -- | otherwise    = Nothing
        | otherwise    = error $ "no '" ++ name ++ "' five-place predicate."

onePlacers, onePlaceStarters, entityonePlacers :: [(String, [Entity])]
onePlaceStarters = [
  ("true",        entities )
  , ("false",     [] )
  , ("role",      [] )

  , ("manager",	 [E,F] )
  , ("individual",	 [E,F,B,G] )
  , ("people",	 [E,F,B,G,M,W] )
  , ("framework",	 [A] )
  , ("company",	 [I] )
  , ("work",	 [A,J] ++ map agent working )
  , ("worker",	 map agent working )

  , ("assertive",	 [] )
  , ("bad",	 [N,Q,O,V,X,Y,K] )
  , ("best_placed",	 [E,F] )

  , ("cause_of",	 [N,O,P,V] )
  , ("characteristic",	 [D,R,T,V] )
  , ("common",	 [X,Y,N,O,V,Q] )
  , ("critically_important",	 [A,D,T] )
  , ("day_to_day",	 [R] )
  , ("difficult",	 [] )
  , ("effective",	 [A] )
  , ("good",	 [T,E,J] )
  , ("helpless",	 [M,W] )
  , ("unsupported",	 [M,W] )
  , ("high",	 [Y] )
  , ("little",	 [D,T] )

  , ("stressful",	 [N,O,Q,V] )
  , ("male",	 [B,M,E] )
  , ("female",	 [G,W,F] )
  , ("strategy",	 [J,K] )
  ]

onePlacers = 
	entityonePlacers ++ onePlaceStarters

predid1 "people"	= predid1 "person"
predid1 "person"	= Just person
predid1 "thing"	= Just thing
predid1 "man"	= predid1 "male"

predid1 "poor"  = predid1 "bad"
predid1 "women"  = predid1 "female"
predid1 "men"  = predid1 "male"

predid1 name = if name `elem` (map fst onePlacers) then
	Just (pred1 (concat [ oneple | (id, oneple) <- onePlacers
		, id == name] ) ) else
		-- Nothing
		error $ "no '" ++ name ++ "' one-place predicate."

entityonePlacers =
	map (\x -> (snd x, [fst x])) entity_check

genonePlacer :: [ (Content, [(Case,Entity)]) ] ->
	String -> String -> Case -> 
	(String, [Entity])
genonePlacer area id content role =
	( id, [ r | (co,cs) <- area
		, co == content
		, Just r <-[lookup role cs]
		] )

type OnePlacePred	= Entity -> Bool
type TwoPlacePred	= Entity -> Entity -> Bool
type ThreePlacePred	= Entity -> Entity -> Entity -> Bool
type FourPlacePred      = Entity -> Entity -> Entity -> Entity -> Bool
type FivePlacePred      = Entity -> Entity -> Entity -> Entity -> Entity ->  Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs	= \ x -> elem x xs

pred1 :: [Entity] -> OnePlacePred
pred1	= flip elem

test1 :: String -> OnePlacePred
test1 p = fromMaybe (\_ -> False) (predid1 p)

person, thing :: OnePlacePred
person	= \ x -> (test1 "male" x || test1 "female" x || test1 "role" x || x == Someone)
thing	= \ x -> (x == Unspec || x == Something || not ( person x ) )

pred2 :: [(Entity,Entity)] -> TwoPlacePred
pred3 :: [(Entity,Entity,Entity)] -> ThreePlacePred
pred2 xs	= curry ( `elem` xs )
pred3 xs	= curry3 ( `elem` xs )
pred4 xs	= curry4 ( `elem` xs )

goal, event, condition, idea, attitude, affiliation :: [ (Content, [(Case, Entity)]) ]
goal = [
	]
event = [
	]
condition = [
	]
idea = [
	]
attitude = [
	]
affiliation = [
	]

gentwoPlacer :: [ (Content, [(Case,Entity)]) ] ->
	String -> String -> Case -> Case ->
	(String, [(Entity,Entity)] )
gentwoPlacer area id content role1 role2 =
	( id, [ (r1,r2) | (co,cs) <- area
		, co == content
		, Just r1 <-[lookup role1 cs]
		, Just r2 <- [lookup role2 cs]
		] )

twoPlacers =
	twoPlaceStarters

possessions	= [(B,J),(T,J),(E,J),(B,X),(T,X),(E,X),(T,G)]
appreciation	= [ (E,Unspec,J) ]
qualities	= [ (B,A),(T,G),(B,X),(T,X),(E,X) ]
conflict	= []
supervision	= [(E,M),(E,W),(F,M),(F,W)]
isBoss	= pred1 $ map fst supervision
isWorker	= pred1 $ map snd supervision

supervisor	= pred1 $ map fst supervision
boss	= supervisor
subordinate	= pred1 $ map snd supervision
employee	= subordinate
manager = boss

strategies = [(W,J),(M,K)]
disappointments = []
disappoint	= pred2 $ disappointments
resent	= pred2 $ map swap disappointments

knowledge	= [(B,I),(G,I),(E,I)]
acquaintances	= []
help	= pred2 $ supervision

twoPlacers, twoPlaceStarters :: [(String, [(Entity,Entity)])]
twoPlaceStarters = [
  ("ask_for", map (\(_,t,r)->(r,t)) giving )
  , ("face_to_face",  [(E,M),(E,W),(F,M),(F,W)])
  , ("know_V2",    knowledge ++ acquaintances ++ map swap acquaintances)
  , ("have",  possessions ++ qualities ++ 
		    strategies ++
		    map (\(_,t,r) -> (r,t)) giving )
  , ("stand", [(C,D),(P,Q),(S,T),(U,V)])
  , ("lack", [(N,D),(O,T)])
  , ("level", [(Y,X)])
  , ("more_vulnerable", [(M,W)])
  , ("more_high", [])
  -- , ("tend_to_report", (forgetful3 . predid3) "talk_about" )
  , ("like",  map (\(a,t,r) -> (a,r)) appreciation)
  , ("work",  [(a,c) | (a,p,c) <- working] )
  , ("placing", [(worker, place) | (worker,_,place) <- working ])
  ]

predid2 name = if name `elem` (map fst twoPlacers) then
	Just (pred2 (concat [ twople | (id, twople) <- twoPlacers
		, id == name] ) ) else
		-- Nothing
		error $ "no '" ++ name ++ "' two-place predicate."

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z	= f (x,y,z)
curry4 f x y z w	= f (x,y,z,w)

genthreePlacer :: [ (Content, [(Case,Entity)]) ] ->
	String -> String -> Case -> Case -> Case ->
	(String, ThreePlacePred)
genthreePlacer area id content role1 role2 role3 =
	( id, pred3 [ (r1,r2,r3) | (co,cs) <- area
		, co == content
		, Just r1 <-[lookup role1 cs]
		, Just r2 <- [lookup role2 cs]
		, Just r3 <- [lookup role3 cs]
		] )

threePlacers, threePlaceStarters :: [(String, ThreePlacePred)]
threePlaceStarters = [
    ]
threePlacers =
	(genthreePlacer event "ask_V2Q" "ask" Agent Recipient Predicate) :
	genthreePlacer event "tell" "state" Agent Recipient Predicate :
	threePlaceStarters

threePlaceStartrs = [
  ("liked", pred3 appreciation )
  , ("ask", pred3 $ map (\(a,t,r) -> (r,a,t)) giving)
  , ("talk", pred3 $ map (\(a,t,r) -> (a,r,t)) comms)
  , ("talk_about", pred3 comms)
  , ("call", pred3 naming)
  , ("get", pred3 $ map (\(a,t,r) -> (r,t,a)) giving)
  , ("give", pred3 $ [(a,r,t) | (a,t,r) <- giving])
  , ("work", pred3 $ [(a,a,c) | (a,p,c) <- working ] )
  ]

type Content = String
data Case = Agent | Asset | Attribute | Beneficiary | Cause | CoAgent |
	CoPatient | CoTheme | Destination | Experiencer | Extent | Goal |
	InitialLocation | Instrument | Location | Material | Patient | Pivot |
	Predicate | Product | Recipient | Reflexive | Result | Source |
	Stimulus | Theme | Time | Topic | Trajectory | Value
  deriving Eq

genfourPlacer :: [ (Content, [(Case,Entity)]) ] ->
	String -> String -> Case -> Case -> Case ->
	Case -> (String, FourPlacePred)
genfourPlacer area id content role1 role2 role3 role4 =
	( id, pred4 [ (r1,r2,r3,r4) | (co,cs) <- area
		, co == content
		, Just r1 <-[lookup role1 cs]
		, Just r2 <- [lookup role2 cs]
		, Just r3 <- [lookup role3 cs]
		, Just r4 <- [lookup role4 cs]
		] )


agent, theme, recipient, location, instrument ::
	(Entity,Entity,Entity) -> Entity
agent (a,_,_) = a
theme (_,t,_) = t
recipient (_,_,r) = r
patient = theme
location = recipient
instrument = recipient
origin	= theme
destination = recipient

--(worker,job,site)
working	= [(B,Unspec,I),(G,Unspec,I),(E,Unspec,Unspec),(F,Unspec,Unspec),(M,Unspec,Unspec),(W,Unspec,Unspec)]
comms	= [ (W,X,E),(W,X,F) ]
--(agent,theme,recipient)
giving	= [ (B,H,E),(I,H,E),(G,H,F),(E,T,W),(F,T,W) ]
--(agent,patient,name)
naming	= [(I,A,A)]
--(agent,origin,destination)

work_where	= pred2 $ map (\x -> (agent x, location x) ) working
work_as = pred2 $ map (\x -> (agent x, theme x) ) working
say	= pred2 $ map (\x->(agent x, theme x) ) comms
ask	= pred2 $ map (\x->(agent x, recipient x) ) comms
ask_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms
talk	= pred3 $ comms ++  map (\(a,t,r) -> (a,r,t) ) comms
talk_about = pred3 $ map (\x->(agent x, recipient x, theme x) ) comms

-- (teacher,school(location),subject,student,degree)
--(person,subject)


gave	= pred3 giving
got	= pred2 $ map (\x -> (recipient x, patient x) ) giving
got_from	= pred3 $ map (\x -> (recipient x, patient x, agent x) ) giving

told	= pred3 comms

recite = pred2 $ map ( \x -> (agent x, theme x) ) comms

fourPlacers, fourPlaceStarters :: [(String, FourPlacePred)]
fourPlaceStarters = [
        ]

fourPlacers =
	fourPlaceStarters

agent4, theme4, recipient4, location4 :: (Entity,Entity,Entity,Entity) -> Entity
agent4 (a,_,_,_) = a
location4 (_,l,_,_) = l
theme4 (_,_,t,_) = t
recipient4 (_,_,_,r) = r
provider4       = recipient4
location4' (_,_,_,l)     = l
mode4   = location4'
purpose4        = location4'
aim4    = purpose4
result4 = recipient4

fivePlacers = [
        ]


agent5, theme5, recipient5, location5 :: (Entity,Entity,Entity,Entity, Entity) -> Entity
-- for schooling
agent5		(a,_,_,_,_) = a
location5	(_,l,_,_,_) = l
theme5		(_,_,t,_,_) = t
destination5 = theme5
recipient5	(_,_,_,r,_) = r
feature5	(_,_,_,_,f) = f
provider5       = location5
result5 = feature5
style5  = recipient5
purpose5        = feature5
aim5    = purpose5
vehicle5        = location5

forgetful5 :: FivePlacePred -> FourPlacePred
forgetful5 r u v w t = or ( map ( r u v w t ) entities )

forgetful4 :: FourPlacePred -> ThreePlacePred
forgetful4 r u v w = or ( map ( r u v w ) entities )

forgetful3 :: ThreePlacePred -> TwoPlacePred
forgetful3 r u v = or ( map ( r u v ) entities )

forgetful2 :: TwoPlacePred -> OnePlacePred
forgetful2 r u = or ( map ( r u ) entities )

passivize :: TwoPlacePred -> OnePlacePred
passivize r     = \ x -> or ( map ( flip  r x ) entities )

passivize3 :: ThreePlacePred -> TwoPlacePred
passivize3 r    = \x y -> or ( map ( \u -> r u x y ) entities )

passivize4 r = \x y z -> or ( map (\u -> r u x y z ) entities )

self ::  (a -> a -> b) -> a -> b
self p	= \ x -> p x x 

-- vim: set ts=2 sts=2 sw=2 noet:
