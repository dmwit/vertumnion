{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable
import Data.Text (Text)
import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import Vertumnion.Shared
import qualified Config as C
import qualified Config.Schema as C
import qualified Config.Macro as C
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

main :: IO ()
main = do
	hSetBuffering stdin LineBuffering
	argumentParser <- mkArgumentParser
	arguments <- execParser argumentParser
	profile <- loadProfile (argProfile arguments) (argTargetOverride arguments)
	let inOrder f = pure . map unHumanOrder . f . S.map humanOrder
	allStates <- case profile of
		Profile { pSortOrder = AscendingOn o } -> pure (toList o)
		Profile { pSortOrder = Ascending, pMajorStates = Just ss } -> inOrder S.toAscList ss
		Profile { pSortOrder = Descending, pMajorStates = Just ss } -> inOrder S.toDescList ss
		_ -> die "The profile specified does not have an explicit list of states in the sort order nor a list of major states."
	source <- case (argSourceOverride arguments, allStates) of
		(Just src, _) -> pure src
		(_, src:_) -> pure src
		_ -> die "The profile's list of states is empty."
	case break (pTarget profile==) (dropWhile (source/=) allStates) of
		(ss, target:_) -> loop (ss ++ [target])
		_ -> die $ "There are no states between " ++ T.unpack source ++ " and " ++ T.unpack (pTarget profile) ++ "."

loop :: [Text] -> IO loop
loop = go [] where
	go past [] = do
		T.putStrLn "STOP"
		go [] (reverse past)
	go past posthistory@(now:future) = do
		s <- T.getLine
		case T.last s of
			c | T.length s == 0 -> do
				hPutStrLn stderr "WARNING: Ignoring empty input line."
				go past posthistory
			'+' -> do
				T.putStrLn (T.init s <> now)
				go (now:past) future
			'-' -> case past of
				[] -> do
					hPutStrLn stderr "WARNING: Ignoring request to back up from first state."
					go past posthistory
				[before] -> do
					T.putStrLn "STOP"
					T.putStrLn before
					go past posthistory
				before:distantPast@(yesterbefore:_) -> do
					T.putStrLn yesterbefore
					go distantPast (before:posthistory)
			'x' -> do
				T.putStrLn "STOP"
				go [] (reverse past ++ posthistory)
			_ -> do
				hPutStrLn stderr $ "WARNING: Ignoring malformed line " ++ show s ++ " that does not end with +, -, or x"
				go past posthistory

data Arguments = Arguments
	{ argProfile :: FilePath
	, argSourceOverride :: Maybe Text
	, argTargetOverride :: Maybe Text
	} deriving (Eq, Ord, Read, Show)

mkArgumentParser :: IO (ParserInfo Arguments)
mkArgumentParser = do
	desc <- usage
	pure $ info (opts <**> helper)
		(  fullDesc
		<> progDesc "Convert game-agnostic split events to game-specific split events."
		<> (footerDoc . Just . Doc.string) desc
		)
	where
	opts = pure Arguments
		<*> strArgument
			(  metavar "PROFILE"
			<> help "A description of the game being played"
			)
		<*> ( optional $ T.pack <$> strOption
				(  long "source"
				<> short 's'
				<> metavar "EVENT"
				<> help "Choose the first event to report (default: smallest in profile order)"
				)
			)
		<*> ( optional $ T.pack <$> strOption
				(  long "target"
				<> short 't'
				<> metavar "EVENT"
				<> help "Override the target given in the profile just for this run"
				)
			)

usage :: IO String
usage = do
	prog <- getProgName
	profDir <- profileDir
	fp <- configName
	pure . unlines . tail $ [undefined
		, prog ++ " accepts generic event notifications on stdin: it reads a line at a time,"
		, "and expects each line to end with one of \"+\", \"-\", or \"x\". Like vertumnion, it"
		, "uses a profile to guide its behavior: it outputs events from the profile in the sort"
		, "order specified. The set of events is taken from the explicitly-specified sort order,"
		, "if it exists, or from the list of major events if not. If neither are listed, "
		, prog ++ " exits unsuccessfully. The meaning of the input lines is as follows:"
		, ""
		, "+\tAdvance to the next event in sequence. If there isn't currently a run going,"
		, "\tstart the run at the first event (or the source, if specified)."
		, "-\tBack up a split. This could be useful if you accidentally advanced the splits"
		, "\ttoo far, or if you actually made negative progress in your speedrun. The timer"
		, "\tautomatically stops once the target is reached, so if you accidentally"
		, "\tadvanced to the target, you're out of luck; that can't be undone."
		, "x\tAbort the run immediately."
		, ""
		, "Any other characters that precede the +, -, or x are passed through verbatim to"
		, "stdout (so that you can give a frame number for games that specify an fps field)."
		, ""
		, "All files processed by " ++ prog ++ " should be in config-value format and use the"
		, "UTF-8 encoding. See here for details:"
		, "http://hackage.haskell.org/package/config-value/docs/Config.html"
		, ""
		, "PROFILE refers to a file in " ++ profDir ++ "."
		, "The profile should conform to the following format."
		, show (C.generateDocs profileSpec)
		, ""
		, "Additionally, the profile must either have an explicit sort order or a list of"
		, "major events."
		]
