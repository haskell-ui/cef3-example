{-# Language OverloadedStrings #-}
module Options (module Options) where

import Options.Applicative
import Control.Monad (void)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Time (NominalDiffTime)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes, fromJust)
import Data.List.Split (splitOn)
import Data.Char (toLower)
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24reads, sRGB24read)
import Data.Colour.Names (readColourName, white)
import Text.PrettyPrint.ANSI.Leijen (vsep)

--------------------------------------------------------------------------------

type Color = Colour Double
data TimerSetup = TimerSetup
   { timerTime  :: Timer
   , timerColor :: Color
   }

data Options = Options
   { optionPort   :: Int
   , optionTimers :: [TimerSetup]
   }

--------------------------------------------------------------------------------

optParser :: Parser Options
optParser = Options
    <$> optPort
    <*> optTimerList
    where
    optPort = option auto $ mconcat
        [ short 'p'
        , long "port"
        , showDefault
        , value 8099
        , metavar "PORT"
        , help "Server port"
        ]
    optTimerList = parseTimers <$> many optTimer
    optTimer = strOption $ mconcat
        [ short 't'
        , long "timer"
        , metavar "TIME(;COLOR)"
        , helpDoc (Just timerHelp) ]
    timerHelp = vsep
        [ "Timer startup string of format hh:mm:ss and"
        , "optional color name or hex color code, ie. #0ba"
        , "For example:"
        , "  --timer 10:03:00 -t 0:5:0;red"
        , "  --timer 00:03:00;#e67b9e" ]

parseTimers :: [String] -> [TimerSetup]
parseTimers = setDefault . catMaybes . zipWith parseSetup palette
    where
    setDefault [] =
        [ parseSetupUnsafe "3:00:00;#00b5ad"
        , parseSetupUnsafe "1:30:00;#e67b9e" ]
    setDefault as = as

palette :: [Color]
palette = cycle $ map sRGB24read
      [ "#00b5ad", "#e67b9e", "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"
      , "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"]

getOptions :: IO Options
getOptions = execParser optionsParserInfo

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optParser <**> helper) $ mconcat
    [ fullDesc, progDesc description
    , footerDoc $ Just explanation ]
    where
    description = "Starts the timer board with specified timers"
    explanation = vsep
        [ "When run without any timers specified the default will be:"
        , "  tmr --timer 3:00:00;#00b5ad"
        , "      --timer 1:30:00;#e67b9e" ]

--------------------------------------------------------------------------------

newtype Timer = Timer NominalDiffTime

mkTimer :: Int -> Int -> Int -> Timer
mkTimer hs ms ss = Timer $ fromIntegral $ h*60*60 + m*60 + s
    where
    h = max 0 hs
    m = max 0 $ min 59 ms
    s = max 0 $ min 59 ss

showTimer :: Timer -> String
showTimer (Timer d) = printf "%02d:%02d:%02d" hs ms ss
    where
    s  = floor d :: Int
    hs = div s (60*60)
    ms = div (s - hs*60*60) 60
    ss = s - (ms*60 + hs*60*60)

parseTime :: String -> Maybe Timer
parseTime time = case splitOn ":" time of
    [hh,mm,ss] -> mkTimer
        <$> readMaybe hh
        <*> readMaybe mm
        <*> readMaybe ss
    _ -> Nothing

--------------------------------------------------------------------------------

parseColor :: String -> Maybe Color
parseColor color = case map toLower color of
    ['#', r, g, b]             -> readRGBMaybe [r,r ,g,g ,b,b ]
    ['#', r, rr, g, gg, b, bb] -> readRGBMaybe [r,rr,g,gg,b,bb]
    other                      -> readColourName other
    where
    readRGBMaybe = listToMaybe . map fst . sRGB24reads

parseColorDef :: Color -> String -> Color
parseColorDef def = fromMaybe def . parseColor

--------------------------------------------------------------------------------

parseSetup :: Color -> String -> Maybe TimerSetup
parseSetup defaultColor setup = case splitOn ";" setup of
    [time, color] -> TimerSetup <$> parseTime time <*> parseColor color
    [time]        -> TimerSetup <$> parseTime time <*> pure defaultColor
    _             -> Nothing

parseSetupUnsafe :: String -> TimerSetup
parseSetupUnsafe = fromJust . parseSetup white

