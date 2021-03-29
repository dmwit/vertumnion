{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Config.Schema (SectionsSpec, ValueSpec)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
import Data.Coerce
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant
import Data.Colour.SRGB
import Data.Containers.ListUtils
import Data.Default
import Data.Foldable
import Data.Functor.Alt ((<!>))
import Data.String
import Data.Tree
import Data.Int
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Multiset (Multiset)
import Data.Ord
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Traversable
import Data.Vector (Vector)
import Data.Void
import Data.Word
import Database.PostgreSQL.Simple (Connection)
import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk
import Numeric.Natural
import Options.Applicative
import System.Clock (Clock(..), TimeSpec(..), getTime)
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.Glib.UTFString
import System.IO
import System.Posix.Process (getProcessID)
import System.Random
import Text.Printf (printf)
import qualified Config as C
import qualified Config.Schema as C
import qualified Config.Macro as C
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Map.Strict as M.S
import qualified Data.Multiset as MS
import qualified Data.Set as S
import qualified Data.Set.Ordered as O
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.FromField as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.PostgreSQL.Simple.FromRow as DB
import qualified Database.PostgreSQL.Simple.ToRow as DB
import qualified Database.PostgreSQL.Simple.Types as DB
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as CM
import qualified Numeric.LinearAlgebra as Matrix
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

main :: IO ()
main = do
	ctx <- initializeContext
	traverse_ (\thread -> forkIO (thread ctx)) [loggingThread, stdinThread, parserThread, heartbeatThread, moduleThread]
	guiThread ctx -- NB not forked
	-- TODO: maybe stay open and start ignoring stdin, so that pipes don't fill up and block producers?

data Context = Context
	{ ctxConfig :: Config
	, ctxProfile :: Profile
	, ctxErrors :: Chan String
	, ctxInput :: Chan (TimeSpec, Text)
	-- TODO: since this is only written from parserThread (right?) it can probably be an IORef
	, ctxUITimerLabelStatus :: MVar TimerLabelStatus
	, ctxUITimerLabel :: Label
	, ctxUIGraph :: DrawingArea
	, ctxModuleInput :: Chan Command
	, ctxModuleOutputs :: TVar [(String, TVar [ModulePoint])]
	, ctxTimeMagnitude :: IORef TimeMagnitude
	, ctxEventStore :: TreeStore Event
	, ctxStateRange :: TVar StateRange
	-- the Word is an epoch, used as an equality-check fast path
	, ctxRandomRuns :: TVar (Word, Runs)
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
	uiTimerLabelStatus <- newMVar (Stopped 0)
	uiTimerLabel <- labelNew (string <$> Nothing)
	uiGraph <- drawingAreaNew
	moduleInput <- newChan
	moduleOutputs <- newTVarIO []
	timeMagnitude <- newIORef (pDurationHint profile)
	eventStore <- treeStoreNew []
	stateRange <- newTVarIO StateRange
		{ srStateCache = Uninitialized
		, srMin = pTarget profile
		, srMax = pTarget profile
		}
	randomRuns <- newTVarIO (0, M.empty)
	pure Context
		{ ctxConfig = config
		, ctxProfile = profile
		, ctxErrors = errors
		, ctxInput = input
		, ctxUITimerLabelStatus = uiTimerLabelStatus
		, ctxUITimerLabel = uiTimerLabel
		, ctxUIGraph = uiGraph
		, ctxModuleInput = moduleInput
		, ctxModuleOutputs = moduleOutputs
		, ctxTimeMagnitude = timeMagnitude
		, ctxEventStore = eventStore
		, ctxStateRange = stateRange
		, ctxRandomRuns = randomRuns
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

type ID = Int32

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

soAllStates :: SortOrder -> Maybe (OSet Text)
soAllStates (AscendingOn ss) = Just ss
-- not a catch-all pattern because I want compiler warnings if constructors get
-- added later
soAllStates Ascending = Nothing
soAllStates Descending = Nothing

ctxAllStates :: Context -> Maybe (OSet Text)
ctxAllStates = soAllStates . pSortOrder . ctxProfile

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
	h <- case mh of
		Right h -> h <$ hSetBuffering h LineBuffering
		Left (_ :: SomeException) -> do
			hPutStrLn stderr $ "WARNING: Could not open log file " ++ fp ++ " for appending. Logging to stderr"
			pure stderr
	hPutStrLn h (show now ++ ": " ++ err)
	forever $ do
		err <- readChan chan
		now <- getCurrentTime
		hPutStrLn h (show now ++ ": " ++ err)

data TimerLabelStatus
	= RunningSince TimeSpec
	| Stopped DiffTimeSpec
	deriving (Eq, Ord, Read, Show)

{-# INLINE string #-}
string :: String -> String
string = id

arbitraryTimeSpec :: TimeSpec
arbitraryTimeSpec = TimeSpec 0 0

guiThread :: Context -> IO ()
guiThread ctx = do
	window <- windowNew
	on window objectDestroy mainQuit
	vbox <- vBoxNew False 0
	set window [windowTitle := string "vertumnion", containerChild := vbox]

	gameLabel <- labelNew . Just . pGame . ctxProfile $ ctx
	-- TODO: Maybe it would be nice for the target to be editable
	targetLabel <- labelNew . Just . ("Target: " <>) . pTarget . ctxProfile $ ctx

	updateTimerLabel ctx

	eventLog <- treeViewNewWithModel (ctxEventStore ctx)
	set eventLog [treeViewHeadersVisible := False]
	let newColumn :: GlibString string => (Event -> IO string) -> IO Int
	    newColumn = addColumnPlainText eventLog (ctxEventStore ctx)
	newColumn (pure . eState)
	newColumn $ \e -> do
		mag <- readIORef (ctxTimeMagnitude ctx)
		pure . snd . showDiffTimeSpec (eMicrosecond e) $ mag
	for_ (pFPS (ctxProfile ctx)) $ \fps -> newColumn $ \e -> do
		mag <- readIORef (ctxTimeMagnitude ctx)
		case eFrame e of
			Nothing -> pure ""
			Just n -> do
				let (mag', content) = showDiffTimeSpec (fromInteger n / realToFrac fps) mag
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

	widgetSetSizeRequest (ctxUIGraph ctx) 200 200
	on (ctxUIGraph ctx) draw (renderGraph ctx)

	legend <- mkLegend
	graphPane <- hPanedNew
	panedPack1 graphPane (ctxUIGraph ctx) True True
	panedPack2 graphPane legend False True

	dataPane <- vPanedNew
	panedPack1 dataPane eventLogScroll True True
	panedPack2 dataPane graphPane False True

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

mkLegend :: IO TreeView
mkLegend = do
	store <- listStoreNew $ []
		++ map Left (zip [0..] allModules)
		++ map Right [("time", [1, 1], 1/2), ("state", [1,3,1,1], -1/2), ("number", [1,5], -5/2)]
	treeView <- treeViewNewWithModel store
	col <- treeViewColumnNew
	graphicRenderer <- cellRendererPixbufNew
	labelRenderer <- cellRendererTextNew
	treeViewColumnPackStart col graphicRenderer False
	treeViewColumnPackStart col labelRenderer True
	cellLayoutSetAttributes col graphicRenderer store $ \row -> [cellPixbuf :=> legendPixbuf row]
	cellLayoutSetAttributes col labelRenderer store $ \row -> [cellText := legendText row]
	treeViewAppendColumn treeView col
	set treeView [treeViewHeadersVisible := False]
	pure treeView

legendText :: Either (Double, Module) (String, [Double], Double) -> String
legendText (Left (_, m)) = mLabel m
legendText (Right (s, _, _)) = s

legendPixbuf :: Either (Double, Module) (String, [Double], Double) -> IO Pixbuf
legendPixbuf row = do
	surface <- Cairo.createImageSurface Cairo.FormatARGB32 30 30
	Cairo.renderWith surface $ case row of
		Left (i, _) -> do
			let dAngle = min (pi / numModules) (pi / 12)
			    angle = 2 * pi * i / numModules
			    RGB r g b = toSRGB (cieLAB d65 50 (100 * cos angle) (100 * sin angle))
			Cairo.setSourceRGB r g b
			Cairo.moveTo 15 15
			Cairo.arc 15 15 14 (angle-dAngle) (angle+dAngle)
			Cairo.fill
		Right (_, intervals, start) -> do
			Cairo.scale 5 5
			Cairo.setLineWidth 0.4
			Cairo.setDash intervals start
			Cairo.moveTo 0 3
			Cairo.lineTo 6 3
			Cairo.stroke
	pixbufNewFromSurface surface 0 0 30 30
	where numModules = fromIntegral (length allModules)

renderGraph :: Context -> Render ()
renderGraph ctx = do
	(_, _, w, h) <- Cairo.clipExtents
	let wScale = w/1.75
	    hScale = h/1.3
	if wScale < hScale
		then Cairo.scale wScale wScale >> Cairo.translate 0.45 (h/(2*wScale)-0.55)
		else Cairo.scale hScale hScale >> Cairo.translate (w/(2*hScale)-0.425) 0.1
	states_ <- liftIO (ctxStates ctx)
	ptss_ <- liftIO $ readTVarIO (ctxModuleOutputs ctx)
	ptss <- liftIO $ for ptss_ $ \(label, ptVar) -> (,) label <$> readTVarIO ptVar

	let
		states :: Map Text Double
		states = M.fromList (zip states_ [0..])

		maxStateIx :: Double
		maxStateIx = fromIntegral (M.size states - 1)

		yStates = [s | (_, pts) <- ptss, ModulePoint { y = State s } <- pts]
		yTimes = [t | (_, pts) <- ptss, ModulePoint { y = Time t } <- pts]
		yLogScales = [n | (_, pts) <- ptss, ModulePoint { y = LogScale n } <- pts]

		dtbX = chooseDiffTimeBounds [x pt | (_, pts) <- ptss, pt <- pts]
		dtbY = chooseDiffTimeBounds yTimes
		lsb = chooseLogScaleBounds yLogScales

		isMajor = case pMajorStates (ctxProfile ctx) of
			Nothing -> const True
			Just ss -> (`S.member` ss)

		xPos :: DiffTimeSpec -> Double
		xPos = dtbPos dtbX

		yPos :: Dependent -> Double
		yPos (State s) = 1 - (states M.! s) / maxStateIx
		yPos (Time t) = 1 - dtbPos dtbY t
		yPos (LogScale n) = 1 - logScalePos lsb n

		numModules :: Double
		numModules = fromIntegral (length ptss)

		hasPoints = not . null . concatMap snd $ ptss
		hasState = not . null $ yStates
		hasDiffTime = not . null $ yTimes
		hasLogScale = not . null $ yLogScales

	Cairo.setLineWidth 0.001
	when hasPoints $ do
		let ds = dtbGridLines dtbX

		-- draw grid lines
		for_ ds $ \d -> do
			let coord = xPos d
			Cairo.moveTo coord 0
			Cairo.lineTo coord 1
		Cairo.stroke

		-- label grid lines
		spaceExtent <- Cairo.textExtents [' ']
		fe <- Cairo.fontExtents
		labels <- traverse sequence [(d, sizedText (dtbLabel dtbX d)) | d <- ds]

		let maxWidth = maximum $ map (Cairo.textExtentsXadvance . stExtents . snd) labels
		    xScale = 0.1/(Cairo.textExtentsXadvance spaceExtent + maxWidth)
		    yScale = 0.1/(Cairo.fontExtentsAscent fe + Cairo.fontExtentsDescent fe)
		    scale = min xScale yScale
		    halfScale = scale/2
		    y = 1.1 + (Cairo.fontExtentsAscent fe - Cairo.fontExtentsDescent fe) * halfScale

		scaleFontMatrix scale
		forM_ labels $ \(d, st) -> do
			let dx = Cairo.textExtentsXadvance (stExtents st) * halfScale
			Cairo.moveTo (xPos d - dx) y
			Cairo.showText (dtbLabel dtbX d)

	when hasState $ do
		-- draw grid lines
		Cairo.setDash [1/60, 3/60, 1/60, 1/60] (-1/120)
		flip M.traverseWithKey states $ \state ix -> when (isMajor state) $ do
			let y = 1 - ix / maxStateIx
			Cairo.moveTo 0 y
			Cairo.lineTo 1 y
		Cairo.stroke

		-- label grid lines
		valignText 1.05 0.2 . M.fromList $
			[ (yPos (State state), ("", "", state))
			| state <- M.keys states
			, isMajor state
			]

	when hasDiffTime $ do
		let ds = dtbGridLines dtbY
		-- draw grid lines
		Cairo.setDash [1/60, 1/60] (1/120)
		for_ ds $ \d -> do
			let coord = yPos (Time d)
			Cairo.moveTo 0 coord
			Cairo.lineTo 1 coord
		Cairo.stroke

		-- label grid lines
		valignText (-0.25) 0.2 . M.fromList $
			[(yPos (Time d), (dtbLabel dtbY d, "", "")) | d <- ds]

	when hasLogScale $ do
		-- draw grid lines
		Cairo.setDash [1/60, 5/60] (-5/120)
		for_ (lsbGridLines lsb) $ \n -> do
			let coord = yPos (LogScale n)
			Cairo.moveTo 0 coord
			Cairo.lineTo 1 coord
		Cairo.stroke

		-- label grid lines
		valignText (-0.4) 0.1 . M.fromList $
			[ (yPos (LogScale gridLine), logScaleShow gridLine)
			| gridLine <- lsbGridLines lsb
			]

	for_ (zip [0..] ptss) $ \(i, (lbl, pts)) -> do
		let dAngle = min (pi / numModules) (pi / 12)
		    angle = 2 * pi * i / numModules
		    RGB r g b = toSRGB (cieLAB d65 50 (100 * cos angle) (100 * sin angle))
		Cairo.setSourceRGB r g b
		for_ pts $ \pt -> do
			let xCoord = xPos (x pt)
			    yCoord = yPos (y pt)
			Cairo.moveTo xCoord yCoord
			Cairo.arc xCoord yCoord 0.03 (angle-dAngle) (angle+dAngle)
		Cairo.fill

	Cairo.setSourceRGB 0 0 0

valignText :: (Cairo.CairoString string, IsString string, Monoid string) =>
	Double -> Double -> Map Double (string, string, string) -> Render ()
valignText x w entries_ = do
	entries <- traverse valignInfo $
		if M.null entries_
		then M.singleton 0 ("", "", "")
		else entries_
	fe <- Cairo.fontExtents

	let maxPreWidth = maximum (vaiPrefixWidth <$> entries)
	    maxSufWidth = maximum (vaiSuffixWidth <$> entries)
	    maxWidth = maxPreWidth + maxSufWidth
	    xScale = w / maxWidth
	    yScale = chooseYScale xScale fe entries
	    scale = min xScale yScale
	    sepX = x + (w + (maxPreWidth - maxSufWidth)*scale)/2
	    dy = (Cairo.fontExtentsAscent fe - Cairo.fontExtentsDescent fe)*scale/2

	scaleFontMatrix scale

	void . flip M.traverseWithKey entries $ \y_ vai -> do
		let y = y_ + dy
		    sepWidth = scale * (Cairo.textExtentsXadvance . stExtents . vaiSeparator) vai
		Cairo.moveTo (sepX - vaiPrefixWidth vai*scale) y
		Cairo.showText (stString (vaiPrefix vai) <> stString (vaiSeparator vai))
		Cairo.moveTo (sepX + sepWidth/2) y
		Cairo.showText (stString (vaiSuffix vai))

scaleFontMatrix :: Double -> Render ()
scaleFontMatrix s = Cairo.setFontMatrix . CM.scalarMultiply s =<< Cairo.getFontMatrix

data SizedText string = SizedText
	{ stString :: string
	, stExtents :: Cairo.TextExtents
	}

sizedText :: Cairo.CairoString string => string -> Render (SizedText string)
sizedText s = SizedText s <$> Cairo.textExtents s

data VAlignInfo string = VAlignInfo
	{ vaiPrefix, vaiSeparator, vaiSuffix :: SizedText string
	, vaiPrefixWidth :: Double
	, vaiSuffixWidth :: Double
	}

valignInfo :: Cairo.CairoString string => (string, string, string) -> Render (VAlignInfo string)
valignInfo (pre_, sep_, suf_) = do
	pre <- sizedText pre_
	sep <- sizedText sep_
	suf <- sizedText suf_
	pure VAlignInfo
		{ vaiPrefix = pre
		, vaiSeparator = sep
		, vaiSuffix = suf
		, vaiPrefixWidth = dx pre + dx sep / 2
		, vaiSuffixWidth = dx suf + dx sep / 2
		}
	where dx = Cairo.textExtentsXadvance . stExtents

chooseYScale :: Double -> Cairo.FontExtents -> Map Double (VAlignInfo string) -> Double
chooseYScale def fe vai
	| M.size vai < 2 = def
	| otherwise = minDY / reqDY
	where
	minDY = minimum . ap (zipWith subtract) tail . M.keys $ vai
	reqDY = Cairo.fontExtentsHeight fe


data DiffTimeBounds = DiffTimeBounds
	{ dtbZero :: DiffTimeSpec
	, dtbGridLine :: DiffTimeSpec
	, dtbOne :: DiffTimeSpec
	, dtbLabel :: DiffTimeSpec -> String
	}

chooseDiffTimeBounds :: [DiffTimeSpec] -> DiffTimeBounds
chooseDiffTimeBounds [] = DiffTimeBounds
	{ dtbZero = 0
	, dtbGridLine = 1
	, dtbOne = 10
	, dtbLabel = \t -> show (floor t) ++ "s"
	}
chooseDiffTimeBounds ts = DiffTimeBounds
	{ dtbZero = smallAndRound
	, dtbGridLine = grid
	, dtbOne = bigAndRound
	, dtbLabel = label
	} where
	small = minimum ts
	big = maximum ts
	width = big - small
	gridTarget = width / 10
	(grid, label) = case M.lookupGE gridTarget roundIntervals of
		Just v -> v
		Nothing -> ( gridDays (60*60*24) (gridTarget / (60*60*24))
		           , \t -> let (pre, sep, suf) = logScaleShow . fromInteger . floor $ t/(60*60*24)
		                   in concat [pre, sep, suf, "d"]
		           )
	gridDays cur tgt
		| tgt > 5 = gridDays (cur*10) (tgt/10)
		| tgt > 2 = cur*5
		| tgt > 1 = cur*2
		| otherwise = cur
	smallAndRound = fromInteger (floor (small / grid)) * grid
	bigAndRound = fromInteger (ceiling (big / grid)) * grid

roundIntervals :: Map DiffTimeSpec (DiffTimeSpec -> String)
roundIntervals = M.fromList $ zip
	[      1,       2,       5,       10,       30 -- seconds
	,   60*1,    60*2,    60*5,    60*10,    60*30 -- minutes
	,60*60*1, 60*60*2, 60*60*6, 60*60*12           -- hours
	]
	([]
	++ replicate 5 (fmt "%m:%02S")
	++ replicate 5 (\t -> fmt (if t < 60*60 then "%-mm" else "%-hh%02Mm") t)
	++ replicate 4 (\t -> fmt (if t < 60*60*24 then "%-hh" else "%-dd%02Hh") t)
	)
	where
	fmt format = formatTime defaultTimeLocale format . ndt

	ndt :: DiffTimeSpec -> NominalDiffTime
	ndt = realToFrac

dtbGridLines :: DiffTimeBounds -> [DiffTimeSpec]
dtbGridLines dtb = [dtbZero dtb, dtbZero dtb + dtbGridLine dtb .. dtbOne dtb]

dtbPos :: DiffTimeBounds -> DiffTimeSpec -> Double
dtbPos dtb d = if o == z then 0.5 else realToFrac ((d - z) / (o - z)) where
	o = dtbOne dtb
	z = dtbZero dtb

data LogScaleBounds = LogScaleBounds
	{ lsbZero :: Double
	, lsbScale :: Double
	, lsbOffset :: Double
	, lsbGridLines :: [Double]
	} deriving (Eq, Ord, Read, Show)

chooseLogScaleBounds :: [Double] -> LogScaleBounds
chooseLogScaleBounds ns
	| null ns = LogScaleBounds
		{ lsbZero = 1
		, lsbScale = recip (log 10)
		, lsbOffset = 0
		, lsbGridLines = [1, 2, 5, 10]
		}
	| all (< 0) ns = let lsb = chooseLogScaleBounds (negate <$> ns) in LogScaleBounds
		{ lsbZero = -lsbZero lsb
		, lsbScale = -lsbScale lsb
		, lsbGridLines = negate <$> lsbGridLines lsb
		, lsbOffset = 1
		}
	| any (<= 0) ns = chooseLogScaleBounds (filter (>0) ns)
	| otherwise = LogScaleBounds
		{ lsbZero = zero
		, lsbScale = recip (log (one/zero))
		, lsbOffset = 0
		, lsbGridLines = gridLines
		} where
		small = minimum ns
		big = maximum ns

		oom x = floor (logBase 10 x)
		logMags@[smallLogMag, bigLogMag] = map oom [small, big]
		mags@[smallMag, bigMag] = map (10^^) logMags
		[smallRatio, bigRatio] = zipWith (/) [small, big] mags

		smallRoundRatio = fromMaybe 1 (S.lookupLE smallRatio roundDigits)
		bigRoundRatio = fromMaybe 10 (S.lookupGE bigRatio roundDigits)

		zero = smallRoundRatio * smallMag
		one = bigRoundRatio * bigMag

		allGrids = S.toAscList . S.fromList $
			[ gridLine
			-- bigLogMag is sometimes too small by 1 due to rounding, so
			-- include 10 as a choice of coefficient
			| ratio <- 10 : S.toList roundDigits
			, mag <- [smallLogMag..bigLogMag]
			, let gridLine = ratio*10^^mag
			, zero <= gridLine && gridLine <= one
			]
		gridLines = chooseAFewEvenly allGrids

chooseAFewEvenly :: [a] -> [a]
chooseAFewEvenly as
	| lines <= 6 = as
	| otherwise = head $ []
		++ [ every (gaps `quot` n) as
		   | n <- [5, 4, 3]
		   , gaps `rem` n == 0
		   ]
		++ [ indices [ (numerator * (lines-1) + 2) `quot` 4
		             | numerator <- [0 .. 4]
		             ]
		   ]
	where
	lines = length as
	gaps = lines-1

	every n [] = []
	every n (x:xs) = x : every n (drop (n-1) xs)

	indices = go 0 as where
		go _ [] _ = []
		go _ _ [] = []
		go i (x:xs) is@(i':is') = case compare i i' of
			LT -> go (i+1) xs is
			EQ -> x : go (i+1) xs is'
			-- should be impossible, but...
			GT -> if i' < 0 then go i (x:xs) is' else go 0 as is

roundDigits :: Set Double
roundDigits = S.fromList [1, 2, 5]

logScalePos :: LogScaleBounds -> Double -> Double
logScalePos lsb n = lsbOffset lsb + log (n/lsbZero lsb) * lsbScale lsb

logScaleShow :: Double -> (String, String, String)
logScaleShow = id
	. snd
	. head
	. sortOn (\(penalty, (pre, mid, suf)) -> (penalty + length (pre++mid++suf), mid))
	-- penalty: non-scientific notation is a bit nicer when it's possible, even
	-- to the point that we might want to choose it even when it's longer
	. (\v -> [(0, showDigitsFlat v), (1, showDigitsScientific v)])
	. dropZeros
	. sigFigs

sigFigs :: Double -> (Int, Int)
sigFigs 0 = (0, 0)
sigFigs n = (round (n/scale), magnitude) where
	-- 0.01 is a fudge factor for slightly-too-low log calculations
	magnitude = floor (logBase 10 (abs n) + 0.01) - 2
	scale = 10^^magnitude

dropZeros :: (Int, Int) -> (Int, Int)
dropZeros v@(mantissa, exponent) = case mantissa `quotRem` 10 of
	(0, 0) -> v
	(mantissa', 0) -> dropZeros (mantissa', exponent+1)
	_ -> v

showDigitsFlat :: (Int, Int) -> (String, String, String)
showDigitsFlat (mantissa, exponent)
	| exponent >= 0 = (show mantissa ++ replicate exponent '0', "", "")
	| otherwise = go mantissa exponent ""
	where
	go 0 e suf = (['-' | mantissa < 0] ++ "0", ".", replicate (-e) '0' ++ suf) -- fast path
	go m 0 suf = (show m, ".", suf)
	go m e suf = let (q, r) = m `quotRem` 10 in go q (e+1) (intToDigit (abs r) : suf)

showDigitsScientific :: (Int, Int) -> (String, String, String)
showDigitsScientific (mantissa, exponent) = go mantissa exponent "" where
	go m e suf
		| abs m < 10 = (show m ++ ['.' | not (null suf)] ++ suf, "e", show e)
		| otherwise = let (q, r) = m `quotRem` 10 in go q (e+1) (intToDigit (abs r) : suf)

-- | The 'Word8' is how many digits there are.
data TimeMagnitude = Seconds | Minutes | Hours | Days Word8 deriving (Eq, Ord, Read, Show)

updateTimerLabel :: Context -> IO ()
updateTimerLabel ctx = postGUIAsync $ do
	status <- readMVar (ctxUITimerLabelStatus ctx)
	case status of
		RunningSince from -> go runningAttrs . (`diffTimeSpec` from) =<< getCurrentTimeSpec
		Stopped dur -> go (if dur == 0 then commonAttrs else stoppedAttrs) dur
	where
	commonAttrs = tail [undefined
		, AttrWeight { paWeight = WeightHeavy, paStart = setLater, paEnd = setLater }
		, AttrSize { paSize = 32, paStart = setLater, paEnd = setLater }
		, AttrFamily { paFamily = "FreeMono,Latin Modern Mono,Noto Sans Mono CJK JP,DejaVu Sans Mono", paStart = setLater, paEnd = setLater }
		]
	runningAttrs = commonAttrs ++ tail [undefined
		, AttrForeground { paColor = Color 0x1000 0x8000 0x0000, paStart = setLater, paEnd = setLater }
		]
	stoppedAttrs = commonAttrs ++ tail [undefined
		, AttrForeground { paColor = Color 0xb000 0x2800 0x3800, paStart = setLater, paEnd = setLater }
		]
	setLater = error "updateTimerLabel attempted to use an attribute without setting its range first. This is a bug."

	go attrs dur = do
		mag <- readIORef (ctxTimeMagnitude ctx)
		let (mag', text) = showDiffTimeSpec dur mag
		labelSetText (ctxUITimerLabel ctx) text
		labelSetAttributes (ctxUITimerLabel ctx) [attr { paStart = 0, paEnd = length text } | attr <- attrs]
		when (mag /= mag') $ do
			writeIORef (ctxTimeMagnitude ctx) mag'
			redrawAllIntervals ctx

updateGraph :: Context -> IO ()
updateGraph = postGUIAsync . widgetQueueDraw . ctxUIGraph

-- 2^63 microseconds is about 300k years, so even though this technically can't
-- represent all the possible TimeSpec differences, it's gonna be good enough
-- for this application.

-- TODO: Read and Show instances that put the decimal point in the right place
newtype DiffTimeSpec = DiffTimeSpec { getMicros :: Int64 }
	deriving (Eq, Ord, Read, Show, Enum, DB.FromField, DB.ToField, NFData)

-- | @fromInteger 1@ is one second
instance Num DiffTimeSpec where
	DiffTimeSpec a + DiffTimeSpec b = DiffTimeSpec (a+b)
	DiffTimeSpec a - DiffTimeSpec b = DiffTimeSpec (a-b)
	DiffTimeSpec a * DiffTimeSpec b = DiffTimeSpec (fromInteger ((toInteger a * toInteger b + 500000) `div` 1000000))
	negate (DiffTimeSpec a) = DiffTimeSpec (negate a)
	abs (DiffTimeSpec a) = DiffTimeSpec (abs a)
	signum (DiffTimeSpec a) = DiffTimeSpec (1000000 * signum a)
	fromInteger n = DiffTimeSpec (fromInteger (1000000 * n))

instance Real DiffTimeSpec where
	toRational (DiffTimeSpec a) = toRational a / 1000000

instance Fractional DiffTimeSpec where
	DiffTimeSpec a / DiffTimeSpec b = DiffTimeSpec ((a*1000000 + b `div` 2) `div` b)
	recip (DiffTimeSpec a) = DiffTimeSpec ((1000000000000 + a `div` 2) `div` a)
	fromRational n = DiffTimeSpec (round (n*1000000))

instance RealFrac DiffTimeSpec where
	properFraction (DiffTimeSpec a) = (fromIntegral q, DiffTimeSpec r) where
		(q, r) = a `quotRem` 1000000

diffTimeSpec :: TimeSpec -> TimeSpec -> DiffTimeSpec
diffTimeSpec (TimeSpec s ns) (TimeSpec s' ns') = DiffTimeSpec $
	1000000*(s-s') + (ns-ns'+500)`div`1000

addTimeSpec :: TimeSpec -> DiffTimeSpec -> TimeSpec
addTimeSpec ts (DiffTimeSpec micros) = ts + fromIntegral (1000*micros)

showDiffTimeSpec :: DiffTimeSpec -> TimeMagnitude -> (TimeMagnitude, String)
showDiffTimeSpec = showNominalDiffTime . realToFrac

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

getCurrentTimeSpec :: IO TimeSpec
getCurrentTimeSpec = getTime MonotonicRaw

stdinThread :: Context -> IO loop
stdinThread (ctxInput -> chan) = forever $ do
	line <- T.getLine
	now <- getCurrentTimeSpec
	writeChan chan (now, line)

data Event = Event
	{ eFrame :: Maybe Integer
	, eMicrosecond :: DiffTimeSpec
	, eState :: Text
	} deriving (Eq, Ord, Read, Show)

data Command = StateChange Event | Stop DiffTimeSpec deriving (Eq, Ord, Read, Show)

stop :: Text
stop = "STOP"

-- TODO: catch EOF and do something sensible (probably just quit?)
-- TODO: catch DB connection errors
parserThread :: Context -> IO loop
parserThread ctx = DB.connectPostgreSQL (db (ctxConfig ctx)) >>= go where
	go conn = case pFPS (ctxProfile ctx) of
		Just{} -> idling
		Nothing -> noFramesRunning Nothing Nothing
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
				broadcast now Event { eFrame = Just 0, eMicrosecond = 0, eState = state }
					Nothing
					(running (negate frame) frame)
					idling
			Left (Just now) -> warnIgnoreStop >> idling
			Left Nothing -> idling

		running dFrame frame runStart splitKey = getEvent >>= \case
			Right (frame', now, state)
				| frame' >= frame -> do
					broadcast runStart
						Event
							{ eFrame = Just (dFrame+frame')
							, eMicrosecond = diffTimeSpec now runStart
							, eState = state
							}
						(Just splitKey)
						(running dFrame frame')
						stopping
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
					broadcast runStart
						Event
							{ eFrame = Just (dFrame'+frame')
							, eMicrosecond = diffTimeSpec now runStart
							, eState = state
							}
						(Just splitKey)
						(running dFrame' frame')
						stopping
			Left (Just now) -> stopTimer (diffTimeSpec now runStart) >> idling
			Left Nothing -> running dFrame frame runStart splitKey

		stopping = getEvent >>= \case
			Left (Just now) -> idling
			_ -> stopping

		-- invariant: arguments are both Just or both Nothing
		noFramesRunning runStartM splitKey = do
			(now, state) <- readChan (ctxInput ctx)
			let runStart = fromMaybe now runStartM
			case (state == stop, ctxValidState ctx state) of
				(True, _) -> do
					maybe warnIgnoreStop (stopTimer . diffTimeSpec now) runStartM
					noFramesRunning Nothing Nothing
				(_, True) -> broadcast runStart
					Event
						{ eFrame = Nothing
						, eMicrosecond = diffTimeSpec now runStart
						, eState = state
						}
					splitKey
					(\rs sk -> noFramesRunning (Just rs) (Just sk))
					noFramesStopping
				_ -> do
					writeChan (ctxErrors ctx)
						(printf "Ignoring unknown state %s at time %s" (show state) (show now))
					noFramesRunning runStartM splitKey

		noFramesStopping = do
			(now, state) <- readChan (ctxInput ctx)
			if state == stop
			then noFramesRunning Nothing Nothing
			else noFramesStopping

		broadcast runStart event splitKey continue won = do
			-- tell the database
			(runID, seqNo) <- case splitKey of
				Just (runID, seqNo) -> pure (runID, seqNo+1)
				Nothing -> do
					[DB.Only runID] <- DB.query conn
						"insert into run (game, fps, started) values (?, ?, now()) returning id"
						(pGame (ctxProfile ctx), pFPS (ctxProfile ctx))
					pure (runID :: ID, 0 :: ID)
			DB.execute conn
				"insert into split (run, seq_no, state, microsecond, frame) values (?, ?, ?, ?, ?)"
				(runID, seqNo, eState event, eMicrosecond event, eFrame event)

			-- tell the UI
			atomically $ do
				sr <- readTVar (ctxStateRange ctx)
				case srInsert (pSortOrder (ctxProfile ctx)) (eState event) sr of
					(Any True, sr') -> writeTVar (ctxStateRange ctx) sr'
					_ -> pure ()
			status <- takeMVar (ctxUITimerLabelStatus ctx)
			putMVar (ctxUITimerLabelStatus ctx) $ case status of
				RunningSince{} -> status
				_ -> RunningSince runStart
			postGUIAsync $ do
				case status of
					RunningSince{} -> pure ()
					_ -> treeStoreClear (ctxEventStore ctx)
				logEvent ctx event

			-- tell the modules
			writeChan (ctxModuleInput ctx) (StateChange event)

			-- tell the parser thread
			if eState event == pTarget (ctxProfile ctx)
			then stopTimer (eMicrosecond event) >> won
			else continue runStart (runID, seqNo)

		stopTimer dur = do
			-- nothing interesting to tell the database
			-- tell the UI
			status <- takeMVar (ctxUITimerLabelStatus ctx)
			putMVar (ctxUITimerLabelStatus ctx) $ case status of
				RunningSince{} -> Stopped dur
				_ -> status
			-- tell the modules
			writeChan (ctxModuleInput ctx) (Stop dur)
			-- the parser thread already knows

		warnIgnoreStop = writeChan (ctxErrors ctx) "Ignoring redundant STOP"

ctxValidState :: Context -> Text -> Bool
ctxValidState ctx s = maybe True (O.member s) (ctxAllStates ctx)

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
	Ascending -> fetchFromDB
	Descending -> fetchFromDB
	where
	fetchFromDB = do
		mConn <- try (DB.connectPostgreSQL (db (ctxConfig ctx)))
		case mConn of
			Left err -> pure (const Nothing (DB.sqlState err {- type-checking hint -}))
			Right conn -> do
				ss <- DB.query conn
					"select distinct state from run join split on run.id = split.run where run.game = ?"
					(DB.Only (pGame (ctxProfile ctx)))
				DB.close conn
				pure . Just . stateCache ctx . fromOnlys $ ss

fromOnlys :: [DB.Only a] -> [a]
fromOnlys = coerce

heartbeatThread :: Context -> IO loop
heartbeatThread ctx = forever $ do
	updateTimerLabel ctx
	threadDelay (1000000`div`30) -- update at 30fps...ish

data Dependent
	= State Text
	| Time DiffTimeSpec
	| LogScale Double
	deriving (Eq, Ord, Read, Show)

instance NFData TimeSpec where
	rnf (TimeSpec secs nsecs) = rnf secs `seq` rnf nsecs

instance NFData Dependent where
	rnf (State s) = rnf s
	rnf (Time t) = rnf t
	rnf (LogScale n) = rnf n

data ModulePoint = ModulePoint
	{ x :: DiffTimeSpec
	, y :: Dependent
	} deriving (Eq, Ord, Read, Show)

instance NFData ModulePoint where
	rnf (ModulePoint xVal yVal) = rnf xVal `seq` rnf yVal

-- | Common case: we've just computed an offset to add to an event's time.
dtDependent :: Event -> DiffTimeSpec -> Dependent
dtDependent e dt = Time (eMicrosecond e + dt)

-- | Common case: we've just computed an offset to add to an event's time.
dtPoint :: Event -> DiffTimeSpec -> ModulePoint
dtPoint Event { eMicrosecond = micro } dt = ModulePoint micro (Time (micro + dt))

data Module = Module
	{ mLabel :: String
	, mLaunch :: Context -> Chan Command -> TVar [ModulePoint] -> IO Void
	}

-- | If your module only ever adds points, and only needs to see the latest
-- event to know which points it wants to add, you can use this wrapper to do
-- some of the bookkeeping for you.
appendOnlyModule :: String -> (Context -> IO a) -> (a -> Command -> IO [ModulePoint]) -> Module
appendOnlyModule label initialize newPoints = Module label $ \ctx i o -> do
	a <- initialize ctx
	let go pts = do
	    	me <- readChan i
	    	dpts <- newPoints a me
	    	let pts' = dpts ++ pts
	    	when (not (null dpts)) $ do
	    		atomically (writeTVar o pts')
	    		updateGraph ctx
	    	go $ case me of
	    		Stop{} -> []
	    		StateChange{} -> pts'
	go []

-- | If you meet the requirements of 'appendOnlyModule', and additionally your
-- new points always occur at the same time as the most recent event, you can
-- use this to add the x-coordinate for you.
synchronousOnlyModule :: String -> (Context -> IO a) -> (a -> Command -> IO [Dependent]) -> Module
synchronousOnlyModule label initialize newDependents
	= appendOnlyModule label initialize $ \a me ->
		map (ModulePoint (meMicrosecond me)) <$> newDependents a me
	where
	meMicrosecond (StateChange e) = eMicrosecond e
	meMicrosecond (Stop t) = t

stateProgressModule :: Module
stateProgressModule = synchronousOnlyModule "state progress" def
	$ \ ~() me -> pure [State (eState e) | StateChange e <- [me]]

pbModule :: Module
pbModule = synchronousOnlyModule "PB" initialize addPBRun
	where
	initialize ctx = do
		pbRef <- newIORef Nothing
		eConn <- try (DB.connectPostgreSQL (db (ctxConfig ctx)))
		case eConn of
			Left err -> writeChan (ctxErrors ctx) $ printf
				"Error connecting to database in pbModule: %s"
				(displayException @DB.SqlError err)
			Right conn -> pure ()
		pure (pbRef, eConn, pGame (ctxProfile ctx), pTarget (ctxProfile ctx))

	addPBRun (_, Left err, _, _) _ = pure []
	addPBRun (pbRef, _, _, _) Stop{} = [] <$ writeIORef pbRef Nothing
	addPBRun a@(pbRef, Right conn, game, tgt) me@(StateChange e) = do
		mPB <- readIORef pbRef
		case mPB of
			Just pb -> pure $ case M.lookup (eState e) pb of
				Nothing -> []
				Just t -> [dtDependent e ((pb M.! tgt) - t)]
			Nothing -> do
				runs <- DB.query @_ @(ID, DiffTimeSpec) conn findPBRunQuery (eState e, tgt, game)
				pb <- case runs of
					[] -> pure M.empty
					(runID, _):_ -> M.fromList <$> DB.query conn
						"select state, min(microsecond) from split where run = '?' group by state"
						(DB.Only runID)
				writeIORef pbRef (Just pb)
				addPBRun a me

	findPBRunQuery = "\
		\select b_run, e_microsecond - b_microsecond as duration \
		\from \
			\(\
				\(\
					\select run as b_run, microsecond as b_microsecond, seq_no as b_seq_no \
					\from split \
					\where state = ? \
				\) as beginning \
				\join \
				\( \
					\select run as e_run, microsecond as e_microsecond, seq_no as e_seq_no \
					\from split \
					\where state = ? \
				\) as ending \
				\on b_run = e_run and b_seq_no < e_seq_no \
			\) as interval \
			\join \
			\run \
			\on interval.b_run = run.id \
		\where run.game = ? \
		\order by duration, b_run \
		\limit 1"

data Split = Split
	{ run :: ID
	, seqNo :: Int32
	, state :: Text
	, microsecond :: DiffTimeSpec
	, frame :: Maybe Int32
	} deriving (Eq, Ord, Read, Show)
	-- BEWARE: some functions assume that the Ord instance compares on run
	-- first, then seqNo, then other stuff

instance DB.FromRow Split where
	fromRow = pure Split
		<*> DB.field
		<*> DB.field
		<*> DB.field
		<*> DB.field
		<*> DB.field

allSplits :: Context -> Connection -> IO [Split]
allSplits (ctxProfile -> p) conn = do
	ids <- DB.query conn "select id from run where game = ?" (DB.Only (pGame p))
	DB.query conn "select run, seq_no, state, microsecond, frame from split where run in ?"
		. DB.Only
		. DB.In
		. fromOnlys @ID
		$ ids

data Segment = Segment
	{ endState :: Text
	, duration :: DiffTimeSpec
	, frames :: Maybe Int32
	} deriving (Eq, Ord, Read, Show)

finishableSegments :: Context -> [Split] -> Map Text [Segment]
finishableSegments ctx splits = id
	. M.fromListWith (++)
	. concat
	. ap (zipWith mkSegment) tail
	$ sorted
	where
	sorted = sort splits
	finishable = unsafeFinishableStates ctx sorted
	mkSegment split split' =
		[(state split, [Segment
			{ endState = state split'
			, duration = microsecond split' - microsecond split
			, frames = liftA2 (-) (frame split') (frame split)
			}])
		| run split == run split'
		, seqNo split + 1 == seqNo split'
		, state split `S.member` finishable
		, state split' `S.member` finishable
		]

-- unsafe because it assumes the provided split list is already sorted
unsafeFinishableStates :: Context -> [Split] -> Set Text
unsafeFinishableStates (ctxProfile -> p) = id
	. restrict
	. dfs (pTarget p)
	. M.fromListWith (++)
	. concat
	. ap (zipWith predecessor) tail
	where
	-- paranoia: what if there are two profiles with the same game name? (but
	-- not quite paranoid enough to deal with two profiles with the same game
	-- name and shared states. at some point you finally have to give in to
	-- GIGO)
	restrict ss = case soAllStates (pSortOrder p) of
		Just ss' -> S.intersection (O.toSet ss') ss
		Nothing -> ss

	predecessor s s' =
		[(state s', [state s]) | run s == run s' && seqNo s + 1 == seqNo s']

dfs :: Ord a => a -> Map a [a] -> Set a
dfs root neighbors = go root (S.singleton root) where
	go s seen = case M.lookup s neighbors of
		Nothing -> seen
		Just (S.fromList -> ss) -> foldr go (ss `S.union` seen) (ss S.\\ seen)

-- This produces the expected (in the statistics sense of the term) run time
-- from all source states that can reach the target to the target. The model
-- used here is that if you are currently in state S0, then the next state you
-- arrive at S1 and the time it takes to get there t are sampled IID from some
-- (unknown) distribution.
--
-- Suppose for a moment that we *did* know that distribution for each state. Then
--
-- E[T → T] = 0
--
-- E⎡S  → T⎤ = ∑ p  * ⎛t   + E[S  → T]⎞  	whenever S  ≠ T
--  ⎣ i    ⎦   j  ij  ⎝ ij      j     ⎠  	          i
--
-- where T is the target state, S  ranges over states that can reach T (as does
--                               i
--
-- S ⎞, p   is the probability of transitioning to state S  given that you are
--  j⎠   ij                                               j
--
-- in state S  right now, and t   is the expected duration of a transition from
--           i                 ij
--
-- S  to S .
--  i     j
--
-- Now in the case where we do *not* know the distribution for each state, we
-- can approximate it by the uniform distribution over transition/duration
-- pairs from previous runs. This gives us estimates of p   and t   and a
--                                                       ij      ij
-- collection of linear equations with E⎡S  → T⎤ as the variables. We can
--                                      ⎣ i    ⎦
-- estimate these expectations by solving the system of linear equations.
expectedRunTimes :: Context -> Connection -> IO (Map Text DiffTimeSpec)
expectedRunTimes ctx conn = do
	splits <- allSplits ctx conn
	let target = pTarget (ctxProfile ctx)
	    stats = summarize <$> finishableSegments ctx splits
	    ss = S.insert target (M.keysSet stats)
	    nonTargets = S.delete target ss -- N.B. (S.delete k . S.insert k) is not necessarily id
	    -- this does all the addition in the world of exact integers, then at
	    -- the last second converts to Double; this avoids rounding issues to
	    -- the extent possible (at the unlikely cost of overflow issues)
	    summarize segs = (\(n, dur) -> (n, realToFrac dur)) <$> M.fromListWith pointwiseAddition
	    	[(endState seg, (1, duration seg)) | seg <- segs]
	    pointwiseAddition (n, dur) (n', dur') = (n+n', dur+dur')
	    outEdgeCounts = sum . fmap fst <$> stats
	    nonTargetsList = S.toList nonTargets
	    transitionMatrix = Matrix.matrix (S.size nonTargets)
	    	[ M.findWithDefault 0 to outEdges / outEdgeCount
	    	| from <- nonTargetsList
	    	, let outEdges = fst <$> M.findWithDefault M.empty from stats
	    	      outEdgeCount = max 1 (M.findWithDefault 1 from outEdgeCounts)
	    	, to <- nonTargetsList
	    	]
	    edgeTimes = Matrix.vector
	    	[ sum
	    		[ M.findWithDefault 0 to outEdges / outEdgeCount
	    		| to <- S.toList ss
	    		]
	    	| from <- nonTargetsList
	    	, let outEdges = snd <$> M.findWithDefault M.empty from stats
	    	      outEdgeCount = max 1 (M.findWithDefault 1 from outEdgeCounts)
	    	]
	when (M.keysSet stats /= nonTargets) . writeChan (ctxErrors ctx) $ printf
		"Some finishable states didn't have any edges to other finishable states or something? Weird. (nonTargets, stats) = %s"
		(show (nonTargets, stats))
	let expectedTimes = (Matrix.ident (S.size nonTargets) - transitionMatrix) Matrix.<\> edgeTimes
	pure
		. M.insert target 0
		. M.fromList
		. zipWith (\s t -> (s, realToFrac t)) nonTargetsList
		$ Matrix.toList expectedTimes

expectedModule :: Module
expectedModule = synchronousOnlyModule "expected" initialize handleEvent where
	initialize ctx = do
		conn <- DB.connectPostgreSQL (db (ctxConfig ctx))
		expectedsRef <- newIORef Nothing
		pure (ctx, conn, expectedsRef)

	handleEvent (ctx, conn, expectedsRef) e = case e of
		Stop{} -> [] <$ writeIORef expectedsRef Nothing
		StateChange e -> do
			expectedsMaybe <- readIORef expectedsRef
			expecteds <- case expectedsMaybe of
				Just expecteds -> pure expecteds
				Nothing -> do
					expecteds <- expectedRunTimes ctx conn
					writeIORef expectedsRef (Just expecteds)
					pure expecteds
			pure . map (dtDependent e) . toList
				$ M.lookup (eState e) expecteds

type Runs = Map Text (Multiset DiffTimeSpec)

runGenerationThread :: Context -> Chan Command -> IO loop
runGenerationThread ctx cmds = do
	conn <- DB.connectPostgreSQL (db (ctxConfig ctx))
	waiting conn
	where
	runTVar = ctxRandomRuns ctx
	target = pTarget (ctxProfile ctx)
	maxRunCount = 1000 -- TODO: configurable

	waiting conn = do
		cmd <- readChan cmds
		case cmd of
			Stop{} -> waiting conn
			StateChange e -> do
				atomically $ modifyTVar runTVar (\(epoch, _) -> (epoch+1, M.empty))
				segs <- finishableSegments ctx <$> allSplits ctx conn
				generating conn segs (eState e)

	generating conn segs state = do
		(_, runs) <- readTVarIO runTVar
		let remainingRunCount = maxRunCount - if state `M.member` segs
		    	then fromIntegral (MS.size (M.findWithDefault MS.empty state runs))
		    	else maxRunCount
		replicateM_ remainingRunCount $ do
			events <- randomRun segs target state
			case events of
				(s,_):_ | s == target -> atomically . modifyTVar' runTVar $
					\(epoch, runs) -> (,) (epoch+1) $! addRun events runs
				_ -> pure ()
		cmd <- readChan cmds
		case cmd of
			Stop{} -> waiting conn
			StateChange e -> generating conn segs (eState e)

randomRun :: Map Text [Segment] -> Text -> Text -> IO [(Text, DiffTimeSpec)]
randomRun segMap_ tgt = go 0 [] where
	segMap = V.fromList <$> segMap_
	go t prev s
		| s == tgt = pure prev'
		| otherwise = case M.lookup s segMap of
			Just segs -> do
				seg <- (segs V.!) <$> randomRIO (0, V.length segs - 1)
				go (t+duration seg) prev' (endState seg)
			Nothing -> pure prev'
		where prev' = (s,t):prev

addRun :: [(Text, DiffTimeSpec)] -> Runs -> Runs
addRun [] runs = runs
addRun es@((_, t):_) runs = M.S.unionWith MS.union newRuns runs where
	-- const keeps the last (earliest) one in the list if we hit a given state
	-- multiple times. If we were paranoid, we could use max instead. We don't
	-- want to keep them both because they are correlated -- and so not IID
	-- samples.
	newRuns = M.fromListWith const
		[(state, MS.singleton (t-t')) | (state, t') <- es]

-- | Simple modules that want access to random runs generated by the
-- 'runGenerationThread' can use this to do the bulk of the bookkeeping for
-- them. They will automatically get incremental updating if generation takes a
-- long time.
withRandomRuns :: String ->
	-- | Initialize some custom data on module launch
	(Context -> IO a) ->
	-- | Initialize some custom data on the first event from a run
	(Context -> a -> Event -> IO b) ->
	-- | Generate some points, given the most up-to-date random runs
	(Context -> a -> b -> Runs -> Event -> IO [ModulePoint]) ->
	Module
withRandomRuns s initializeLaunch initializeRun generate = Module s $ \ctx i_ o -> do
	(epoch, _) <- readTVarIO (ctxRandomRuns ctx)

	i <- newChan
	forkIO . forever $ readChan i_ >>= writeChan i . Right
	forkIO $ awaitEpochChanges (ctxRandomRuns ctx) i epoch

	a <- initializeLaunch ctx
	waiting ctx i o a
	where
	awaitEpochChanges i_ i epoch = do
		(epoch', runs) <- atomically $ do
			v@(epoch', runs) <- readTVar i_
			when (epoch == epoch') retry
			pure v
		writeChan i (Left runs)
		awaitEpochChanges i_ i epoch'

	waiting ctx i o a = do
		modification <- readChan i
		case modification of
			Left{} -> waiting ctx i o a
			Right Stop{} -> waiting ctx i o a
			Right (StateChange e) -> do
				b <- initializeRun ctx a e
				running ctx i o a b M.empty [e]

	running ctx i o a b runs events = do
		modification <- readChan i
		case modification of
			Left runs' -> redraw ctx i o a b runs' events
			Right (StateChange event) -> redraw ctx i o a b runs (event:events)
			Right Stop{} -> waiting ctx i o a

	redraw ctx i o a b runs events = do
		pts <- concat <$> traverse (generate ctx a b runs) events
		pts `deepseq` atomically (writeTVar o pts)
		updateGraph ctx
		running ctx i o a b runs events

percentileModule :: Double -> Module
percentileModule p_ = withRandomRuns (pString ++ " percentile")
	(\_ -> pure ())
	(\_ _ _ -> pure ())
	$ \_ _ _ runs event -> pure . toList $ do
		dts <- M.lookup (eState event) runs
		dt <- dts MS.!? round (p * fromIntegral (MS.size dts - 1))
		pure $ dtPoint event dt
	where
	p = min 1 (max 0 p_)

	pString = if fromIntegral pBigInt == pBig
		then show pBigInt ++ ordinalSuffix pBigInt
		else show pBig ++ "th"
		where
		pBig = 100*p
		pBigInt = round pBig
	ordinalSuffix 1 = "st"
	ordinalSuffix 2 = "nd"
	ordinalSuffix 3 = "rd"
	ordinalSuffix _ = "th"

pbChanceModule :: Module
pbChanceModule = withRandomRuns "attempts to PB"
	(DB.connectPostgreSQL . db . ctxConfig)
	getPBInfo
	(\_ _ -> computeAttempts)
	where
	getPBInfo (ctxProfile -> p) conn e = do
		pbLengths <- DB.query conn
			"select ending.microsecond - beginning.microsecond as duration \
			\from (select id from run where game = ?) as this_game \
			\join (select run, seq_no, microsecond from split where state = ?) as beginning \
			\on this_game.id = beginning.run \
			\join (select run, seq_no, microsecond from split where state = ?) as ending \
			\on this_game.id = ending.run and beginning.seq_no <= ending.seq_no \
			\order by duration \
			\limit 1"
			(pGame p, eState e, pTarget p)
		pure (fromOnlys pbLengths)

	computeAttempts pbLengths runs event = pure $ do
		pbLength <- pbLengths
		Just dts <- [M.lookup (eState event) runs]
		let nLT = MS.countLT (pbLength - eMicrosecond event) dts
		guard (eMicrosecond event < pbLength)
		guard (nLT > 0)
		pure ModulePoint
			{ x = eMicrosecond event
			, y = LogScale $ fromIntegral (MS.size dts) / fromIntegral nLT
			}

-- TODO: make this configurable
allModules :: [Module]
allModules = [stateProgressModule, percentileModule 0.1, percentileModule 0.9, expectedModule, pbModule, pbChanceModule]

moduleThread :: Context -> IO loop
moduleThread ctx = do
	(chans, outs) <- fmap unzip . for allModules $ \m -> do
		chan <- newChan
		tvar <- newTVarIO []
		forkIO (() <$ mLaunch m ctx chan tvar)
		pure (chan, (mLabel m, tvar))
	atomically $ writeTVar (ctxModuleOutputs ctx) outs
	-- TODO: when modules are configurable, only launch this thread when it's
	-- needed
	chan <- newChan
	forkIO (runGenerationThread ctx chan)
	forever $ do
		mEvent <- readChan (ctxModuleInput ctx)
		for_ (chan:chans) $ \chan -> writeChan chan mEvent
