module Main where

import Communication
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
	gr <- readPGF ( path ++ "/Communication.pgf" )
	let lang = languages gr
	let morpho = buildMorpho gr $ head lang
	hClose stderr
	hDuplicateTo stdout stderr
	s <- getLine
	let l = (chomp . lc_first) s
	let unknown = unwords (morphoMissing morpho (words l))
	putStrLn ("Unknown_words: " ++ unknown )
	let ps = parses gr l
	let ls = map (linear gr <=< transform) ps
	putStrLn ("Parsed: " ++ show (map (showExpr []) ps ) )
	let urs = map (unmaybe . rep) ps
	-- let reps = map (\ur -> ur (term2ref drsRefs var_e)) urs
	-- putStrLn ("Representation: " ++ show reps )
	-- let lfs = map (\ur -> drsToLF (ur (term2ref drsRefs var_e))) urs
	-- putStrLn ("LF: " ++ show lfs )
	putStrLn ("Answer: No answer" )
	let courses = map (label . fg) ps
	putStrLn ("Course: " ++ foldl takeCourse "Unparseable" courses )

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
	) (concatMap (splitOn " , " ) (splitOn " or " (a ++ " , " ++ b)))

formatUp es = let parts = splitAt 1 (reverse es)
	in case snd parts of 
		[] -> concat (fst parts)
		_ -> concat  (intersperse " , " (snd parts) ++ [" or "] ++ fst parts )
--
-- vim: set ts=2 sts=2 sw=2 noet:
