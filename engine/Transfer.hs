module Main where

import Chat
import LogicalForm hiding ((==))
import Evaluation

--import Model
import WordsCharacters

import Data.Maybe
import Control.Monad
import Data.List.Split
import Data.List

import GHC.IO.Handle
import System.IO

import System.Environment.FindBin

main :: IO ()
main = do
	path <- getProgPath
	gr <- readPGF ( path ++ "/Chat.pgf" )
	hClose stderr
	hDuplicateTo stdout stderr
	s <- getLine
	let l = (chomp . lc_first) s
	putStrLn ("Unknown_words: " ++ (unknown l) )
	let ps = parses gr l
	let ls = map ((linear gr) <=< transform) ps
	putStrLn ("Parsed: " ++ (show (map (showExpr []) ps) ) )
	let urs = map (unmaybe . rep) ps
	-- let reps = map (\ur -> ur (term2ref drsRefs var_e)) urs
	-- putStrLn ("Representation: " ++ show reps )
	-- let lfs = map (\ur -> drsToLF (ur (term2ref drsRefs var_e))) urs
	-- putStrLn ("LF: " ++ show lfs )
	putStrLn ("Answer: " ++ (bestAnswer ls) )
	let courses = map (label . fg) ps
	putStrLn ("Course: " ++ foldl takeCourse "Unparseable" courses )

unknown :: String -> String
unknown ws = intercalate ", " ( filter (\x -> not (checkLists x ws) ) (words ws))

checkLists :: String -> String -> Bool
checkLists w ws	=	if check_on_wordlist [w] then True
									else if check_on_wordlist (alternatives w (bigram ws)) then True
									else if check_on_wordlist (alternatives w (trigram ws)) then True
									else False
splitVariants :: [String] -> [String]
splitVariants ls = concat $ map (splitOn ", ") ls

check_on_wordlist :: [String] -> Bool
check_on_wordlist = any (flip elem (splitVariants wordlist))

alternatives :: String -> [(String, String)] -> [String]
alternatives w bis	= [ bi | (key,bi) <- bis, key==w]

bigram :: String -> [ (String, String) ]
bigram ws = let zs = zip ss sss
						where
						ss = words ws
						sss = tail ss
						in (map (\(a,b) -> (a, unwords [a,b]) ) zs) ++
							(map (\(a,b) -> (b, unwords [a,b]) ) zs)

trigram :: String -> [ (String, String) ]
trigram ws = let zs = zip3 ss sss ssss
			where
			ss = words ws
			sss = tail ss
			ssss = tail sss
			in (map (\(a,b,c) -> (a, unwords [a,b,c]) ) zs) ++
				(map (\(a,b,c) -> (c, unwords [a,b,c]) ) zs)

label :: GUtt -> String
label (GQUt (GPosQ (GWH_Pred _ _)))	= "WH"
label (GQUt (GNegQ (GWH_Pred _ _)))	= "WH"
label (GQUt (GPosQ (GWH_ClSlash _ _)))	= "WH"
label (GQUt (GNegQ (GWH_ClSlash _ _)))	= "WH"
label (GQUt (GPosQ (GYN _)))	= "YN"
label (GQUt (GNegQ (GYN _)))	= "YN"
label (GQUt (GPosQ (GTagQ _ _)))	= "Tag"
label (GQUt (GNegQ (GTagQ _ _)))	= "Tag"
label (GQUt (GPosQ (GTagComp _ _)))	= "Tag"
label (GQUt (GNegQ (GTagComp _ _)))	= "Tag"
label _				= "Unparseable"

takeCourse :: String -> String -> String
takeCourse _ "WH" = "WH"
takeCourse "WH" _ = "WH"
takeCourse _ "YN" = "YN"
takeCourse "YN" _ = "YN"
takeCourse _ "Tag"  = "Tag"
takeCourse "Tag" _  = "Tag"
takeCourse _ "S"  = "S"
takeCourse "S" _  = "S"
takeCourse "Unparseable" _  = "Unparseable"
takeCourse _  _   = error "undefined course, not WH, YN, S, or Unparseable"

bestAnswer :: [Maybe String] -> String
bestAnswer ss = 
	foldl takeAnswer "No answer" (map (fromMaybe "No answer") ss)

takeAnswer :: String -> String -> String
takeAnswer _ "yes" = "yes"
takeAnswer "yes" _ = "yes"
takeAnswer _ "no" = "no"
takeAnswer "no" _  = "no"
takeAnswer a b@('M' : 'a' : _)  = collateAnswer a b -- Mandy
takeAnswer a b@('A' : 'l' : _)  = collateAnswer a b -- Alice
takeAnswer a b@('A' : 'r' : _)  = collateAnswer a b -- Ariel
takeAnswer a b@('S' : 'a' : _)  = collateAnswer a b -- Sabrina
takeAnswer "none" _ = "none of Mandy, Alice, Ariel or Sabrina"
takeAnswer _ "none" = "none of Mandy, Alice, Ariel or Sabrina"
takeAnswer "No answer" _ = "No answer"
takeAnswer _ "No answer" = "No answer"
takeAnswer _  _   = error "undefined answer, not Yes, No, Mandy, Alice, Ariel or Sabrina, none or No answer"

collateAnswer a b = formatUp $ nub $ filter
	(\x -> x ==	"Mandy"
	|| x ==	"Alice"
	|| x ==	"Ariel"
	|| x ==	"Sabrina"
	) (concat $ map (splitOn " , " ) (splitOn " or " (a ++ " , " ++ b)))

formatUp es = let parts = splitAt 1 (reverse es)
	in case (snd parts) of 
		[] -> concat (fst parts)
		_ -> concat  ((intersperse " , " (snd parts)) ++ [" or "] ++ (fst parts) )
--
-- vim: set ts=2 sts=2 sw=2 noet:
