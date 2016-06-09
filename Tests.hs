module Tests where

import Control.Monad
import Data.Maybe

import Data.DRS

import PGF
import Communication
import Representation
import Evaluation
import Model
import WordsCharacters

-- handler gr core tests = putStr $ unlines $ map (\(x,y) -> x++show y) $ zip (map (++"\t") tests ) ( map (\string -> map (\x -> core ( x) ) (parse gr (mkCId "DicksonEng") (startCat gr) string)) tests )

-- import System.Environment.FindBin

gr :: IO PGF
gr = readPGF "./Communication.pgf"

langs :: IO [Language]
langs = liftM languages gr

lang :: IO Language
lang = liftM head langs

morpho :: IO Morpho
morpho = liftM2 buildMorpho gr lang

liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c
liftOp f a b = a >>= \a' -> return (f a' b)

miss :: [String] -> IO [String]
miss ws =
	liftOp morphoMissing morpho ws

ans tests = do
  gr	<- readPGF "./Communication.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ls = map (map ( (linear gr) <=< transform ) ) ps
  let zs = zip (map (++"\t") tests) ls
  putStrLn (unlines (map (\(x,y) -> x ++ (show $ unwords (map displayResult y))) zs) )

displayResult = fromMaybe "Nothing"

trans tests = do
  gr	<- readPGF "./Communication.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ls = map id ps
  let zs = zip (map (++"\t") tests) (map (map (showExpr []) ) ps)
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

reps tests = do
  gr	<- readPGF "./Communication.pgf"
  let ss = map (chomp . lc_first) tests
  let ps = map ( parses gr ) ss
  let ts = map (map (\x -> (((unmaybe . rep) x) (term2ref drsRefs var_e) ))) ps
  let zs = zip (map (++"\t") tests) ts
  putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

lf tests = do
	gr	<- readPGF "./Communication.pgf"
	let ss = map (chomp . lc_first) tests
	let ps = map ( parses gr ) ss
	let ts = map (map (\p -> drsToLF (((unmaybe . rep) p) (DRSRef "r1"))) ) ps
	let zs = zip (map (++"\t") tests) ts
	putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

fol tests = do
	gr	<- readPGF "./Communication.pgf"
	let ss = map (chomp . lc_first) tests
	let ps = map ( parses gr ) ss
	let ts = map (map (\p -> drsToFOL ( (unmaybe . rep) p (term2ref drsRefs var_e) ) ) ) ps
	let zs = zip (map (++"\t") tests) ts
	putStrLn (unlines (map (\(x,y) -> x ++ (show y ) ) zs) )

dic_test = [

      "The second tension is one that really relates to inter-personal relations. "
			, "The second tension is the tension between empathy and assertiveness."
			, "What Mnookin means by empathy is the ability to describe your understanding of the other person's needs , interests and concerns in a non-judgmental way."
			, "By assertiveness, Mnookin means the capacity to articulate your own needs and interests and concerns in a persuasive way."
      , "Some people, for example, some of Mnookin's law students, are terrific at assertion."
			, "Mnookin's law students are very good at making arguments."
			, "But Mnookin's law students are lousy listeners."
			, "Indeed, when Mnookin's law students listen to someone, it's always, 'Yes, but' kind of listening, where they're composing in their own mind what their rebuttal's going to be."

      , "On the other hand, some people are terrific at putting themselves in the other person's shoes."
		  , "However, the problem of people who are terrific in putting themselves in the other person's shoes is that sometimes they can fall into accommodation mode."
			, "Once these people really understand the other side's perspective, they may lose the ability to stand up and defend what their own interests are."

      , "So, when Mnookin calls this managing a tension, what he really means is that people have to learn how to do both well."
			, "With students of negotiation, what Mnookin often does, as a negotiation coach, is help them expand their repertoire so they can do both."

  ]

-- vim: set ts=2 sts=2 sw=2 noet:
