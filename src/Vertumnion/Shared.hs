{-# LANGUAGE OverloadedStrings #-}
module Vertumnion.Shared where

import Config.Schema (SectionsSpec, ValueSpec)
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Functor.Alt ((<!>))
import Data.Maybe
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Text (Text)
import Data.Word
import Numeric.Natural
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.IO
import qualified Config as C
import qualified Config.Schema as C
import qualified Config.Macro as C
import qualified Data.Set as S
import qualified Data.Set.Ordered as O
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

data SortOrder = Ascending | Descending | AscendingOn (OSet Text)
	deriving (Eq, Ord, Read, Show)

-- | The 'Word8' is how many digits there are.
data TimeMagnitude = Seconds | Minutes | Hours | Days Word8 deriving (Eq, Ord, Read, Show)

data Profile = Profile
	{ pGame :: Text
	, pFPS :: Maybe Double
	, pTarget :: Text
	, pMajorStates :: Maybe (Set Text)
	, pSortOrder :: SortOrder
	, pDurationHint :: TimeMagnitude
	} deriving (Eq, Ord, Read, Show)

-- TODO: uh, system paths?
configPath :: FilePath -> IO FilePath
configPath name = (</> name) <$> getUserConfigDir "vertumnion"

profileDir :: IO FilePath
profileDir = configPath "profiles"

configName :: IO FilePath
configName = configPath "config"

loadProfile :: String -> Maybe Text -> IO Profile
loadProfile profile targetOverride = do
	fp <- (</> profile) <$> profileDir
	h <- openFile fp ReadMode
	p <- loadSchema fp h profileSpec
	sanityCheckProfile p
	pure p { pTarget = fromMaybe (pTarget p) targetOverride }

loadSchema :: FilePath -> Handle -> C.ValueSpec a -> IO a
loadSchema fp h spec = do
	hSetEncoding h utf8
	t <- T.hGetContents h
	v <- case C.parse t of
		Left err -> die . (("Error while parsing " ++ fp ++ ":\n") ++) . displayException $ err
		Right v -> pure (C.FilePosition fp <$> v)
	hClose h
	either (die . displayException) pure (C.loadValue spec v)

sanityCheckProfile :: Profile -> IO ()
sanityCheckProfile p
	| Just tags <- soAllStates (pSortOrder p)
	, Just states <- pMajorStates p
	, any (`O.notMember` tags) states = fail $ ""
		++ "Error in profile: these major states do not appear in the sort order:"
		++ [ c
		   | state <- S.toAscList states
		   , state `O.notMember` tags
		   , c <- "\n\t" ++ T.unpack state
		   ]
sanityCheckProfile p
	| Just tags <- soAllStates (pSortOrder p)
	, pTarget p `O.notMember` tags
	= fail "Error in profile: target does not appear in the sort order"
sanityCheckProfile p = pure ()

soAllStates :: SortOrder -> Maybe (OSet Text)
soAllStates (AscendingOn ss) = Just ss
-- not a catch-all pattern because I want compiler warnings if constructors get
-- added later
soAllStates Ascending = Nothing
soAllStates Descending = Nothing

defaultableSection :: Text -> Text -> ValueSpec a -> Text -> SectionsSpec (Maybe a)
defaultableSection sec atom spec help = join <$> C.optSection' sec
	(   (Nothing <$ C.atomSpec atom)
	<!> (Just <$> spec)
	) (help <> " (default: " <> atom <> ")")

profileSpec :: ValueSpec Profile
profileSpec = C.sectionsSpec "profile" $ pure Profile
	<*> C.reqSection' "game" textSpec "Which game you are playing (will be used as database key)"
	<*> defaultableSection "fps" "variable" C.anySpec "The typical framerate of the game"
	<*> C.reqSection' "target" textSpec "The winning state for this game, to stop the timer at"
	<*> defaultableSection "major" "all" statesSpec "A list of major states that you want to be given special status in the UI. As a rule of thumb, pick the states that every run has to go through to win"
	<*> (fromMaybe Ascending <$> C.optSection' "order" orderSpec "When the UI must list states in order, what order shall it use? If you give a list of states, they will be put in exactly that order and other states will be discarded (default: ascending)")
	<*> (fromMaybe Seconds <$> C.optSection' "duration" durationSpec "A hint about how long full runs typically take. If this is a number n, that means 10^n days (default: seconds)")
	where
	textSpec = C.anyAtomSpec <!> C.textSpec
	statesSpec = S.fromList <$> C.listSpec textSpec
	indexMap = AscendingOn . O.fromList
	orderSpec =
		    (Ascending <$ C.atomSpec "ascending")
		<!> (Descending <$ C.atomSpec "descending")
		<!> (indexMap <$> C.listSpec textSpec)
	durationSpec =
		    (Seconds <$ C.atomSpec "seconds")
		<!> (Minutes <$ C.atomSpec "minutes")
		<!> (Hours <$ C.atomSpec "hours")
		<!> (Days <$> C.anySpec)

data HumanOrder = HumanOrder
	{ unHumanOrder :: Text
	, parsedHumanOrder :: [Either Natural Text]
	}

humanOrder :: Text -> HumanOrder
humanOrder t = HumanOrder t (map parse groups) where
	groups = T.groupBy (\c c' -> isDigit c == isDigit c') t
	parse t = case T.decimal t of
		Right (n, t') | T.null t' -> Left n
		_ -> Right t

instance Eq HumanOrder where h == h' = unHumanOrder h == unHumanOrder h'
instance Ord HumanOrder where compare h h' = compare (parsedHumanOrder h) (parsedHumanOrder h')
instance Read HumanOrder where readsPrec n s = map (first humanOrder) (readsPrec n s)
instance Show HumanOrder where show = show . unHumanOrder
