{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Config.Schema (SectionsSpec, ValueSpec)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
import Data.Default
import Data.Foldable
import Data.Functor.Alt ((<!>))
import Data.Int
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Word
import Graphics.UI.Gtk
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
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.Types as DB

main :: IO ()
main = do
	ctx <- initializeContext
	traverse_ (\thread -> forkIO (thread ctx)) [loggingThread, stdinThread, parserThread, heartbeatThread]
	guiThread ctx -- NB not forked

data Context = Context
	{ ctxConfig :: Config
	, ctxProfile :: Profile
	, ctxErrors :: Chan String
	, ctxInput :: Chan (UTCTime, Text)
	-- TODO: since this is only written from parserThread (right?) it can probably be an IORef
	, ctxUITimerLabelStatus :: MVar TimerLabelStatus
	, ctxUITimerLabel :: Label
	, ctxTimeMagnitude :: IORef TimeMagnitude
	, ctxEventStore :: ListStore Event
	}

initializeContext :: IO Context
initializeContext = do
	args <- getArgs
	profileName <- case args of
		["--help"] -> usage stdout >> exitWith ExitSuccess
		["-h"] -> usage stdout >> exitWith ExitSuccess
		["--", f] -> pure f
		[f] -> pure f
		_ -> usage stderr >> exitWith (ExitFailure 1)
	profile <- loadProfile profileName
	config <- loadConfig
	errors <- newChan
	input <- newChan
	initGUI
	uiTimerLabelStatus <- newMVar BlankTimer
	uiTimerLabel <- labelNew (string <$> Nothing)
	timeMagnitude <- newIORef (pDurationHint profile)
	eventStore <- listStoreNew []
	pure Context
		{ ctxConfig = config
		, ctxProfile = profile
		, ctxErrors = errors
		, ctxInput = input
		, ctxUITimerLabelStatus = uiTimerLabelStatus
		, ctxUITimerLabel = uiTimerLabel
		, ctxTimeMagnitude = timeMagnitude
		, ctxEventStore = eventStore
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

-- TODO: provide a lightweight mechanism for overriding the target
data Profile = Profile
	{ pGame :: Text
	, pFPS :: Maybe Double
	, pTarget :: Text
	, pMajorStates :: Maybe (Set Text)
	, pSortOrder :: SortOrder
	, pDurationHint :: TimeMagnitude
	} deriving (Eq, Ord, Read, Show)

data SortOrder = Ascending | Descending | AscendingOn (Map Text Int)
	deriving (Eq, Ord, Read, Show)

-- | Sort numbers numerically, not lexically; e.g. this will consider "99" < "100"
humanSort :: [Text] -> [Text]
humanSort = sortOn (T.groupBy (\c c' -> isDigit c == isDigit c'))

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
	indexMap states = AscendingOn . M.fromList $ zip states [0..]
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

usage :: Handle -> IO ()
usage h = do
	prog <- getProgName
	dir <- profileDir
	fp <- configName
	hPutStr h . unlines . tail $ [undefined
		, "USAGE: " ++ prog ++ " [--] PROFILE"
		, "Run a splits timer. Events are accepted on stdin."
		, ""
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
	| any (`M.notMember` tags) states = fail $ ""
		++ "Error in profile: these major states do not appear in the sort order:"
		++ [ c
		   | state <- S.toAscList states
		   , state `M.notMember` tags
		   , c <- "\n\t" ++ T.unpack state
		   ]
sanityCheckProfile Profile { pTarget = state, pSortOrder = AscendingOn tags }
	| state `M.notMember` tags = fail "Error in profile: target does not appear in the sort order"
sanityCheckProfile p = pure p

loadConfig :: IO Config
loadConfig = do
	fp <- configName
	mh <- try (openFile fp ReadMode)
	case mh of
		Left (e :: IOError) -> pure def
		Right h -> loadSchema fp h configSpec

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

	eventLogScroll <- scrolledWindowNew Nothing Nothing
	set eventLogScroll [containerChild := eventLog, scrolledWindowHscrollbarPolicy := PolicyNever]
	-- TODO: set minimum height to (length (major events) + 1) * height of one row

	boxPackStart vbox gameLabel PackNatural 0
	boxPackStart vbox targetLabel PackNatural 0
	boxPackStart vbox (ctxUITimerLabel ctx) PackNatural 0
	boxPackStart vbox eventLogScroll PackGrow 0
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
		, AttrWeight { paWeight = WeightHeavy }
		, AttrSize { paSize = 32 }
		]
	runningAttrs = commonAttrs ++ tail [undefined
		, AttrForeground { paColor = Color 0x1000 0x8000 0x0000 }
		]
	stoppedAttrs = commonAttrs ++ tail [undefined
		, AttrForeground { paColor = Color 0xb000 0x2800 0x3800 }
		]

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

data ParserState = ParserState
	{ psRun :: Maybe Int32
	, psConn :: DB.Connection
	} deriving Eq

newtype Defaultable a = Defaultable (Maybe a) deriving (Eq, Ord, Read, Show)
instance DB.ToField a => DB.ToField (Defaultable a) where
	toField (Defaultable Nothing) = DB.toField DB.Default
	toField (Defaultable (Just a)) = DB.toField a

-- TODO: catch EOF and do something sensible (probably just quit?)
-- TODO: we need a third state, where we are ignoring events and waiting for a STOP
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
			else broadcast Event { eFrame = Nothing, eTime = now, eState = state }
				splitKey
				(noFrames . Just)
				(stopping (noFrames Nothing))

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
			status <- takeMVar (ctxUITimerLabelStatus ctx)
			putMVar (ctxUITimerLabelStatus ctx) $ case status of
				RunningSince{} -> status
				_ -> RunningSince (eTime event)
			postGUIAsync $ do
				case status of
					RunningSince{} -> pure ()
					_ -> listStoreClear (ctxEventStore ctx)
				() <$ listStoreAppend (ctxEventStore ctx) event

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
	AscendingOn tags -> M.member s tags
	_ -> True

roundUpToPowerOf2 :: Integer -> Integer
roundUpToPowerOf2 n
	| n <= 1 = 1
	| otherwise = go 1 2
	where
	go exp pow
		| n <= pow = head [bit (e+1) | e <- [exp, exp-1 .. exp `rem` 2], testBit n e]
		| otherwise = go (2*exp) (pow*pow)

heartbeatThread :: Context -> IO ()
heartbeatThread ctx = forever $ do
	updateTimerLabel ctx
	threadDelay (1000000`div`30) -- update at 30fps...ish
