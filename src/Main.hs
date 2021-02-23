{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Config.Schema (SectionsSpec, ValueSpec)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
import Data.Containers.ListUtils
import Data.Default
import Data.Foldable
import Data.Functor.Alt ((<!>))
import Data.Tree
import Data.Int
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Word
import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk
import Numeric.Natural
import Options.Applicative
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.Glib.UTFString
import System.IO
import System.Posix.Process (getProcessID)
import Text.Printf (printf)
import qualified Config as C
import qualified Config.Schema as C
import qualified Config.Macro as C
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Set.Ordered as O
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

main :: IO ()
main = do
	ctx <- initializeContext
	traverse_ (\thread -> forkIO (thread ctx)) [loggingThread, stdinThread, parserThread, heartbeatThread]
	guiThread ctx -- NB not forked
	-- TODO: maybe stay open and start ignoring stdin, so that pipes don't fill up and block producers?

data Context = Context
	{ ctxConfig :: Config
	, ctxProfile :: Profile
	, ctxErrors :: Chan String
	, ctxInput :: Chan (UTCTime, Text)
	-- TODO: since this is only written from parserThread (right?) it can probably be an IORef
	, ctxUITimerLabelStatus :: MVar TimerLabelStatus
	, ctxUITimerLabel :: Label
	, ctxTimeMagnitude :: IORef TimeMagnitude
	, ctxEventStore :: TreeStore Event
	, ctxStateRange :: TVar StateRange
	}

initializeContext :: IO Context
initializeContext = do
	argumentParser <- mkArgumentParser
	args <- execParser argumentParser
	profile_ <- loadProfile (argProfile args)
	let profile = profile_ { pTarget = fromMaybe (pTarget profile_) (argTargetOverride args) }
	config <- loadConfig
	errors <- newChan
	input <- newChan
	initGUI
	uiTimerLabelStatus <- newMVar BlankTimer
	uiTimerLabel <- labelNew (string <$> Nothing)
	timeMagnitude <- newIORef (pDurationHint profile)
	eventStore <- treeStoreNew []
	stateRange <- newTVarIO StateRange
		{ srStateCache = Uninitialized
		, srMin = pTarget profile
		, srMax = pTarget profile
		}
	pure Context
		{ ctxConfig = config
		, ctxProfile = profile
		, ctxErrors = errors
		, ctxInput = input
		, ctxUITimerLabelStatus = uiTimerLabelStatus
		, ctxUITimerLabel = uiTimerLabel
		, ctxTimeMagnitude = timeMagnitude
		, ctxEventStore = eventStore
		, ctxStateRange = stateRange
		}

data Config = Config
	{ db :: ByteString
	} deriving (Eq, Ord, Read, Show)

instance Default Config where
	def = Config
		{ db = "dbname=vertumnion"
		}

configSpec :: C.ValueSpec Config
configSpec = C.sectionsSpec
	"configuration"
	(config <$> C.optSection "db" help)
	where
	config Nothing = def
	config (Just t) = Config
		{ db = T.encodeUtf8 t -- I know, I know. I don't care, it will never come up
		}
	help = "A postgres connection string for storing splits (default: dbname=vertumnion)"

data Profile = Profile
	{ pGame :: Text
	, pFPS :: Maybe Double
	, pTarget :: Text
	, pMajorStates :: Maybe (Set Text)
	, pSortOrder :: SortOrder
	, pDurationHint :: TimeMagnitude
	} deriving (Eq, Ord, Read, Show)

data SortOrder = Ascending | Descending | AscendingOn (OSet Text)
	deriving (Eq, Ord, Read, Show)

soCompare :: SortOrder -> Text -> Text -> Ordering
soCompare Ascending t t' = compare t t'
soCompare Descending t t' = compare t' t
soCompare (AscendingOn ts) t t' = case (O.findIndex t ts, O.findIndex t' ts) of
	(Just i, Just i') -> compare i i'
	_ -> compare t t'

soLt :: SortOrder -> Text -> Text -> Bool
soLt so t t' = soCompare so t t' == LT

soMin :: SortOrder -> Text -> Text -> Text
soMin so t t' = if soLt so t t' then t else t'

soMax :: SortOrder -> Text -> Text -> Text
soMax so t t' = if soLt so t t' then t' else t

defaultableSection :: Text -> Text -> ValueSpec a -> Text -> SectionsSpec (Maybe a)
defaultableSection sec atom spec help = join <$> C.optSection' sec
	(   (Nothing <$ C.atomSpec atom)
	<!> (Just <$> spec)
	) (help <> " (default: " <> atom <> ")")

profileSpec :: C.ValueSpec Profile
profileSpec = C.sectionsSpec "profile" $ pure Profile
	<*> C.reqSection' "game" textSpec "Which game you are playing (will be used as database key)"
	<*> defaultableSection "fps" "variable" C.anySpec "The typical framerate of the game"
	<*> C.reqSection' "target" textSpec "The winning state for this game, to stop the timer at"
	<*> defaultableSection "major" "all" statesSpec "A list of major states that you want to be given special status in the UI. As a rule of thumb, pick the states that every run have to go through to win"
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

-- TODO: uh, system paths?
configPath :: FilePath -> IO FilePath
configPath name = (</> name) <$> getUserConfigDir "vertumnion"

profileDir :: IO FilePath
profileDir = configPath "profiles"

configName :: IO FilePath
configName = configPath "config"

logDir :: IO FilePath
logDir = getUserStateDir "vertumnion"

loadSchema :: FilePath -> Handle -> C.ValueSpec a -> IO a
loadSchema fp h spec = do
	hSetEncoding h utf8
	t <- T.hGetContents h
	v <- case C.parse t of
		Left err -> die . (("Error while parsing " ++ fp ++ ":\n") ++) . displayException $ err
		Right v -> pure (C.FilePosition fp <$> v)
	hClose h
	either (die . displayException) pure (C.loadValue spec v)

loadProfile :: String -> IO Profile
loadProfile profile = do
	fp <- (</> profile) <$> profileDir
	h <- openFile fp ReadMode
	p <- loadSchema fp h profileSpec
	sanityCheckProfile p

sanityCheckProfile :: Profile -> IO Profile
sanityCheckProfile Profile { pMajorStates = Just states, pSortOrder = AscendingOn tags }
	| any (`O.notMember` tags) states = fail $ ""
		++ "Error in profile: these major states do not appear in the sort order:"
		++ [ c
		   | state <- S.toAscList states
		   , state `O.notMember` tags
		   , c <- "\n\t" ++ T.unpack state
		   ]
sanityCheckProfile Profile { pTarget = state, pSortOrder = AscendingOn tags }
	| state `O.notMember` tags = fail "Error in profile: target does not appear in the sort order"
sanityCheckProfile p = pure p

loadConfig :: IO Config
loadConfig = do
	fp <- configName
	mh <- try (openFile fp ReadMode)
	case mh of
		Left (e :: IOError) -> pure def
		Right h -> loadSchema fp h configSpec

data Arguments = Arguments
	{ argProfile :: FilePath
	, argTargetOverride :: Maybe Text
	} deriving (Eq, Ord, Read, Show)

mkArgumentParser :: IO (ParserInfo Arguments)
mkArgumentParser = do
	desc <- usage
	pure $ info (opts <**> helper)
		(  fullDesc
		<> progDesc "Run a splits timer. Events are accepted on stdin."
		<> (footerDoc . Just . Doc.string) desc
		)
	where
	opts = pure Arguments
		<*> strArgument
			(  metavar "PROFILE"
			<> help "A description of the game being played"
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
	dir <- profileDir
	fp <- configName
	pure . unlines . tail $ [undefined
		, "PROFILE refers to a file in " ++ dir ++ "."
		, "The profile should be in config-value format and use the UTF-8 encoding."
		, "See here for details:"
		, "http://hackage.haskell.org/package/config-value/docs/Config.html"
		, ""
		, "The profile should conform to the following format."
		, show (C.generateDocs profileSpec)
		, ""
		, "A configuration file is also read from " ++ fp ++ "."
		, "It is similarly a UTF-8 encoded config-value file, with the following format."
		, show (C.generateDocs configSpec)
		]

loggingThread :: Context -> IO loop
loggingThread (ctxErrors -> chan) = do
	-- by reading from the channel first, we guarantee not to write a log unless there's something to put in it
	err <- readChan chan
	now <- getCurrentTime
	me <- getProcessID
	dir <- logDir
	let fp = dir </> show me <.> "log"
	mh <- try (createDirectoryIfMissing True dir >> openFile fp AppendMode)
	case mh of
		Left (_ :: SomeException) -> do
			hPutStrLn stderr $ "WARNING: Could not open log file " ++ fp ++ " for appending. Logging to stderr"
			go stderr
		Right h -> do
			hSetBuffering h LineBuffering
			hPutStrLn h (show now ++ ": " ++ err)
			go h
	where
	go h = forever $ do
		err <- readChan chan
		now <- getCurrentTime
		hPutStrLn h (show now ++ ": " ++ err)

data TimerLabelStatus
	= BlankTimer
	| RunningSince UTCTime
	| Stopped UTCTime UTCTime
	deriving (Eq, Ord, Read, Show)

{-# INLINE string #-}
string :: String -> String
string = id

arbitraryTime :: UTCTime
arbitraryTime = UTCTime (fromOrdinalDate 0 0) 0

guiThread :: Context -> IO ()
guiThread ctx = do
	window <- windowNew
	on window objectDestroy mainQuit
	vbox <- vBoxNew False 0
	set window [windowTitle := string "wow", containerChild := vbox]

	gameLabel <- labelNew . Just . pGame . ctxProfile $ ctx
	-- TODO: Maybe it would be nice for the target to be editable
	targetLabel <- labelNew . Just . ("Target: " <>) . pTarget . ctxProfile $ ctx

	updateTimerLabel ctx

	eventLog <- treeViewNewWithModel (ctxEventStore ctx)
	let newColumn :: GlibString string => (Event -> IO string) -> IO Int
	    newColumn = addColumnPlainText eventLog (ctxEventStore ctx)
	newColumn (pure . eState)
	newColumn $ \e -> do
		status <- readMVar (ctxUITimerLabelStatus ctx)
		mag <- readIORef (ctxTimeMagnitude ctx)
		pure $ case status of
			BlankTimer -> "???"
			RunningSince t -> snd (showInterval t (eTime e) mag)
			Stopped t _ -> snd (showInterval t (eTime e) mag)
	for_ (pFPS (ctxProfile ctx)) $ \fps -> newColumn $ \e -> do
		mag <- readIORef (ctxTimeMagnitude ctx)
		case eFrame e of
			Nothing -> pure ""
			Just n -> do
				let (mag', content) = showNominalDiffTime (fromInteger n / realToFrac fps) mag
				when (mag /= mag') $ do
					writeIORef (ctxTimeMagnitude ctx) mag'
					redrawAllIntervals ctx
				pure content

	-- TODO: set minimum height to (length (major events) + 1) * height of one row
	-- Hmm... from reading online a bit, there doesn't seem to be a really
	-- great way to do this. Approaches:
	--
	-- * widgetSetSizeRequest -- can't resize the widget below this size
	-- * scrolledWindowMinContentHeight -- can't resize the widget below this size
	-- * windowSetDefaultSize -- kinda want to use the computed size of the
	--   other widgets in the window, but that computation isn't done until the
	--   window is shown, and after that the default size is ignored
	-- * treeStoreInsertForest (to add blank rows and reserve space) -- the
	--   ScrolledWindow doesn't pay attention to its child's size at all
	-- * sizeRequest -- this is an invalid signal for ScrolledWindows
	--
	-- Possible way forward: record what size the user put the window at last
	-- time they used this profile.
	eventLogScroll <- scrolledWindowNew Nothing Nothing
	set eventLogScroll [containerChild := eventLog, scrolledWindowHscrollbarPolicy := PolicyNever]
	eventLogAdjustment <- scrolledWindowGetVAdjustment eventLogScroll
	-- TODO: disable jumping when position has been manually changed
	onAdjChanged eventLogAdjustment $
		adjustmentGetUpper eventLogAdjustment >>= adjustmentSetValue eventLogAdjustment

	graph <- drawingAreaNew
	widgetSetSizeRequest graph 200 200
	on graph draw (renderGraph ctx)

	dataPane <- vPanedNew
	panedPack1 dataPane eventLogScroll True True
	panedPack2 dataPane graph False True

	boxPackStart vbox gameLabel PackNatural 0
	boxPackStart vbox targetLabel PackNatural 0
	boxPackStart vbox (ctxUITimerLabel ctx) PackNatural 0
	boxPackStart vbox dataPane PackGrow 0
	widgetShowAll window
	mainGUI

addColumnPlainText ::
	( TreeViewClass treeView
	, TreeModelClass (model row)
	, TypedTreeModelClass model
	, GlibString string
	) =>
	treeView -> model row -> (row -> IO string) -> IO Int
addColumnPlainText treeView model showRow = do
	col <- treeViewColumnNew
	renderer <- cellRendererTextNew
	treeViewColumnPackEnd col renderer False
	cellLayoutSetAttributes col renderer model $ \row -> [cellText :=> showRow row]
	treeViewAppendColumn treeView col

renderGraph :: Context -> Render ()
renderGraph ctx = do
	(_, _, w, h) <- Cairo.clipExtents
	if w < h
		then Cairo.translate 0 ((h-w)/2) >> Cairo.scale w w
		else Cairo.translate ((w-h)/2) 0 >> Cairo.scale h h
	states <- liftIO (ctxStates ctx)
	-- TODO: from here on out everything is terrible
	eForest <- liftIO (treeStoreGetForest (ctxEventStore ctx))
	let es = postOrder eForest
	when (hay $ drop 1 es) $ do
		let a = head es
		    z = last es
		Cairo.setLineWidth 0.01
		Cairo.scale (1/realToFrac (diffUTCTime (eTime z) (eTime a))) (1/fromIntegral (length states))
		for_ es $ \e -> do
			Cairo.arc
				(realToFrac (diffUTCTime (eTime e) (eTime a)))
				((fromIntegral . fromJust . findIndex (eState e ==)) states)
				0.01
				0
				(2*pi)
			Cairo.stroke

treeStoreGetForest :: TreeStore a -> IO (Forest a)
treeStoreGetForest s = do
	n <- treeModelIterNChildren s Nothing
	traverse (treeStoreGetTree s . pure) [0..n-1]

postOrder :: Forest a -> [a]
postOrder = concatMap postOrderTree where postOrderTree (Node a as) = postOrder as ++ [a]

hay :: [a] -> Bool
hay = not . null

data TimeMagnitude = Seconds | Minutes | Hours | Days Word8 deriving (Eq, Ord, Read, Show)

updateTimerLabel :: Context -> IO ()
updateTimerLabel ctx = postGUIAsync $ do
	status <- readMVar (ctxUITimerLabelStatus ctx)
	case status of
		BlankTimer -> go commonAttrs arbitraryTime arbitraryTime
		RunningSince from -> go runningAttrs from =<< getCurrentTime
		Stopped from to -> go stoppedAttrs from to
	where
	commonAttrs = tail [undefined
		, AttrWeight { paWeight = WeightHeavy, paStart = setLater, paEnd = setLater }
		, AttrSize { paSize = 32, paStart = setLater, paEnd = setLater }
		]
	runningAttrs = commonAttrs ++ tail [undefined
		, AttrForeground { paColor = Color 0x1000 0x8000 0x0000, paStart = setLater, paEnd = setLater }
		]
	stoppedAttrs = commonAttrs ++ tail [undefined
		, AttrForeground { paColor = Color 0xb000 0x2800 0x3800, paStart = setLater, paEnd = setLater }
		]
	setLater = error "updateTimerLabel attempted to use an attribute without setting its range first. This is a bug."

	go attrs from to = do
		mag <- readIORef (ctxTimeMagnitude ctx)
		let (mag', text) = showInterval from to mag
		labelSetText (ctxUITimerLabel ctx) text
		labelSetAttributes (ctxUITimerLabel ctx) [attr { paStart = 0, paEnd = length text } | attr <- attrs]
		when (mag /= mag') $ do
			writeIORef (ctxTimeMagnitude ctx) mag'
			redrawAllIntervals ctx

-- TODO: leap seconds lmao
showInterval :: UTCTime -> UTCTime -> TimeMagnitude -> (TimeMagnitude, String)
showInterval old new = showNominalDiffTime (diffUTCTime new old)

showNominalDiffTime :: NominalDiffTime -> TimeMagnitude -> (TimeMagnitude, String)
showNominalDiffTime dur mag_ = (mag, formatTime defaultTimeLocale (formatString mag) dur) where
	mag = max mag_ (timeMagnitude dur)

timeMagnitude :: NominalDiffTime -> TimeMagnitude
timeMagnitude t
	| t < 60 = Seconds
	| t < 60*60 = Minutes
	| t < 24*60*60 = Hours
	| otherwise = Days (go (10*24*60*60) 1)
	where
	go t' d
		| t < t' = d
		-- technically could overflow, but like... 10^255 days is a really lot of days
		| otherwise = go (10*t') (1+d)

formatString :: TimeMagnitude -> String
formatString Seconds = "%03ES"
formatString Minutes = "%02M:" ++ formatString Seconds
formatString Hours = "%02H:" ++ formatString Minutes
formatString (Days n) = "%0" ++ show n ++ "dd " ++ formatString Hours

-- TODO
redrawAllIntervals :: Context -> IO ()
redrawAllIntervals _ = pure ()

stdinThread :: Context -> IO loop
stdinThread (ctxInput -> chan) = forever $ do
	line <- T.getLine
	now <- getCurrentTime
	writeChan chan (now, line)

data Event = Event
	{ eFrame :: Maybe Integer
	, eTime :: UTCTime
	, eState :: Text
	} deriving (Eq, Ord, Read, Show)

data Command = StateChange Event | Stop UTCTime deriving (Eq, Ord, Read, Show)

stop :: Text
stop = "STOP"

-- TODO: catch EOF and do something sensible (probably just quit?)
-- TODO: catch DB connection errors
parserThread :: Context -> IO loop
parserThread ctx = DB.connectPostgreSQL (db (ctxConfig ctx)) >>= go where
	go conn = case pFPS (ctxProfile ctx) of
		Just{} -> idling
		Nothing -> noFrames Nothing
		where
		getEvent = do
			(now, t) <- readChan (ctxInput ctx)
			let (frame_, preState) = T.break (' '==) t
			    state = T.drop 1 preState
			case (T.signed T.decimal frame_, T.null preState) of
				(Right (frame, T.null -> True), False)
					| state == stop -> pure $ Left (Just now)
					| ctxValidState ctx state -> pure $ Right (frame, now, state)
					| otherwise -> Left Nothing <$ writeChan (ctxErrors ctx)
						(printf "Ignoring unknown state %s at frame %d, time %s" (show state) frame (show now))
				_ -> Left Nothing <$ writeChan (ctxErrors ctx) ("Ignoring malformed state change " ++ show t)

		idling = getEvent >>= \case
			Right (frame, now, state) -> do
				broadcast Event { eFrame = Just 0, eTime = now, eState = state }
					Nothing
					(running (negate frame) frame)
					idling
			Left (Just now) -> warnIgnoreStop >> idling
			Left Nothing -> idling

		running dFrame frame splitKey = getEvent >>= \case
			Right (frame', now, state)
				| frame' >= frame -> do
					broadcast Event { eFrame = Just (dFrame+frame'), eTime = now, eState = state }
						(Just splitKey)
						(running dFrame frame')
						(stopping idling)
				| otherwise -> do
					let ddFrame = roundUpToPowerOf2 (max frame (if signed then -1-frame' else frame))
					    signed = frame' < 0
					    dFrame' = dFrame + if signed then shiftL ddFrame 1 else ddFrame
					writeChan (ctxErrors ctx) $ printf
						"New frame %d is smaller than old frame %d. Assuming this is due to %s overflow at %d."
						frame'
						frame
						(if signed then string "signed" else string "unsigned")
						ddFrame
					broadcast Event { eFrame = Just (dFrame'+frame'), eTime = now, eState = state }
						(Just splitKey)
						(running dFrame' frame')
						(stopping idling)
			Left (Just now) -> stopTimer now >> idling
			Left Nothing -> running dFrame frame splitKey

		stopping k = getEvent >>= \case
			Left (Just now) -> k
			_ -> stopping k

		noFrames splitKey = do
			(now, state) <- readChan (ctxInput ctx)
			if state == stop
			then do
				if isJust splitKey then stopTimer now else warnIgnoreStop
				noFrames Nothing
			else if ctxValidState ctx state
			then broadcast Event { eFrame = Nothing, eTime = now, eState = state }
				splitKey
				(noFrames . Just)
				(stopping (noFrames Nothing))
			else do
				writeChan (ctxErrors ctx)
					(printf "Ignoring unknown state %s at time %s" (show state) (show now))
				noFrames splitKey

		broadcast event splitKey continue won = do
			-- tell the database
			(runID, seqNo) <- case splitKey of
				Just (runID, seqNo) -> pure (runID, seqNo+1)
				Nothing -> do
					[DB.Only runID] <- DB.query conn
						"insert into run (game, fps) values (?, ?) returning id"
						(pGame (ctxProfile ctx), pFPS (ctxProfile ctx))
					pure (runID :: Int32, 0 :: Int32)
			DB.execute conn
				"insert into split (run, seq_no, state, moment, frame) values (?, ?, ?, ?, ?)"
				(runID, seqNo, eState event, eTime event, eFrame event)

			-- tell the UI
			atomically $ do
				sr <- readTVar (ctxStateRange ctx)
				case srInsert (pSortOrder (ctxProfile ctx)) (eState event) sr of
					(Any True, sr') -> writeTVar (ctxStateRange ctx) sr'
					_ -> pure ()
			status <- takeMVar (ctxUITimerLabelStatus ctx)
			putMVar (ctxUITimerLabelStatus ctx) $ case status of
				RunningSince{} -> status
				_ -> RunningSince (eTime event)
			postGUIAsync $ do
				case status of
					RunningSince{} -> pure ()
					_ -> treeStoreClear (ctxEventStore ctx)
				logEvent ctx event

			-- tell the parser thread
			if eState event == pTarget (ctxProfile ctx)
			then stopTimer (eTime event) >> won
			else continue (runID, seqNo)

		stopTimer now = do
			-- nothing interesting to tell the database
			-- tell the UI
			status <- takeMVar (ctxUITimerLabelStatus ctx)
			putMVar (ctxUITimerLabelStatus ctx) $ case status of
				RunningSince before -> Stopped before now
				_ -> status
			-- the parser thread already knows

		warnIgnoreStop = writeChan (ctxErrors ctx) "Ignoring redundant STOP"

ctxValidState :: Context -> Text -> Bool
ctxValidState ctx s = case pSortOrder (ctxProfile ctx) of
	AscendingOn tags -> O.member s tags
	_ -> True

roundUpToPowerOf2 :: Integer -> Integer
roundUpToPowerOf2 n
	| n <= 1 = 1
	| otherwise = go 1 2
	where
	go exp pow
		| n <= pow = head [bit (e+1) | e <- [exp, exp-1 .. exp `rem` 2], testBit n e]
		| otherwise = go (2*exp) (pow*pow)

logEvent :: Context -> Event -> IO ()
logEvent ctx e = do
	n <- treeModelIterNChildren store Nothing
	case pMajorStates (ctxProfile ctx) of
		Just major | eState e `S.member` major -> do
			es <- removeMinorEvents major [] (n-1)
			n' <- treeModelIterNChildren store Nothing
			treeStoreInsertTree store [] n' $ Node e [Node e' [] | e' <- es]
		_ -> treeStoreInsert store [] n e
	where
	store = ctxEventStore ctx
	removeMinorEvents major minor n
		| n < 0 = pure minor
		| otherwise = do
			e' <- treeStoreGetValue store [n]
			if eState e' `S.member` major
			then pure minor
			else do
				treeStoreRemove store [n]
				removeMinorEvents major (e':minor) (n-1)

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

class Ord a => IsText a where
	fromText :: Text -> a
	toText :: a -> Text

instance IsText Text where
	fromText = id
	toText = id

instance IsText HumanOrder where
	fromText = humanOrder
	toText = unHumanOrder

instance IsText a => IsText (Down a) where
	fromText = Down . fromText
	toText (Down a) = toText a

data StateCache where
	Ordered :: OSet Text -> StateCache
	Sorted :: IsText a => Set a -> StateCache
	Uninitialized :: StateCache

stateCache :: Context -> [Text] -> StateCache
stateCache ctx ss = case pSortOrder (ctxProfile ctx) of
	Ascending -> Sorted . S.fromList . map humanOrder $ ss
	Descending -> Sorted . S.fromList . map (Down . humanOrder) $ ss
	AscendingOn o -> Ordered o

data StateRange = StateRange
	{ srStateCache :: StateCache
	, srMin :: Text
	, srMax :: Text
	}

srInsert :: SortOrder -> Text -> StateRange -> (Any, StateRange)
srInsert so t sr = pure StateRange
	<*> case srStateCache sr of
		Sorted ts -> (Any (S.notMember t' ts), Sorted (S.insert t' ts)) where t' = fromText t
		_ -> (Any False, srStateCache sr) -- slightly optimistic... if it's Ordered, assumes t appears in the OSet
	<*> (Any (soLt so t (srMin sr)), soMin so t (srMin sr))
	<*> (Any (soLt so (srMax sr) t), soMax so t (srMax sr))

ctxStates :: Context -> IO [Text]
ctxStates ctx = do
	sr <- readTVarIO (ctxStateRange ctx)
	case srStateCache sr of
		Ordered ss -> case (O.findIndex (srMin sr) ss, O.findIndex (srMax sr) ss) of
			(Just iMin, Just iMax) -> pure [fromJust (ss `O.elemAt` i) | i <- [iMin .. iMax]]
			_ -> failedRange sr $ printf "StateRange bounds %s-%s not found in StateCache %s"
				(show (srMin sr))
				(show (srMax sr))
				(show ss)
		Sorted ss -> case (S.lookupIndex eMin ss, S.lookupIndex eMax ss) of
			(Just iMin, Just iMax) -> pure
				. map toText
				. S.toAscList
				. S.take (iMax-iMin+1)
				. S.drop iMin
				$ ss
			_ -> failedRange sr $ printf "StateRange bounds %s-%s not found in StateCache %s"
				(show (srMin sr))
				(show (srMax sr))
				(show (S.map toText ss))
			where
			eMin = fromText (srMin sr)
			eMax = fromText (srMax sr)
		Uninitialized -> do
			mCache <- initializeStateCache ctx
			cache <- case mCache of
				Nothing -> do
					failedRange sr "Could not contact database to initialize state cache"
					pure (stateCache ctx [srMin sr, srMax sr])
				Just cache -> pure cache
			atomically $ do
				sr' <- readTVar (ctxStateRange ctx)
				writeTVar (ctxStateRange ctx) (sr' { srStateCache = cache })
			ctxStates ctx
	where
	failedRange sr err = do
		writeChan (ctxErrors ctx) err
		pure $ [srMin sr | srMin sr /= srMax sr] ++ [srMax sr]

initializeStateCache :: Context -> IO (Maybe StateCache)
initializeStateCache ctx = case pSortOrder (ctxProfile ctx) of
	AscendingOn ss -> pure (Just (Ordered ss))
	_ -> do
		mConn <- try (DB.connectPostgreSQL (db (ctxConfig ctx)))
		case mConn of
			Left err -> pure (const Nothing (DB.sqlState err {- type-checking hint -}))
			Right conn -> do
				ss <- DB.query conn
					"select state from run join split on run.id = split.run where run.game = ? group by state"
					(DB.Only (pGame (ctxProfile ctx)))
				DB.close conn
				pure . Just . stateCache ctx . map DB.fromOnly $ ss

heartbeatThread :: Context -> IO ()
heartbeatThread ctx = forever $ do
	updateTimerLabel ctx
	threadDelay (1000000`div`30) -- update at 30fps...ish
