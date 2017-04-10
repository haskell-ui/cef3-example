{-# Language NoImplicitPrelude #-}
{-# Language LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Protolude
import Prelude (String)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Environment
import System.FilePath (dropFileName)
import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI
import Graphics.CEF3.Simple
import Data.Colour.SRGB (sRGB24show)

import Options
import qualified View

--------------------------------------------------------------------------------

main :: IO ()
main = handleSubProcess $ do
    opts <- getOptions
    startServer opts
    startBrowserUrl $ "http://127.0.0.1:" ++ show (optionPort opts)

startServer :: Options -> IO ()
startServer opts = do
    ep <- dropFileName <$> getExecutablePath
    let config = defaultConfig
            { jsPort   = Just $ optionPort opts
            , jsStatic = Just $ ep ++ "/static"  }
    tickEvent <- newTickEvent
    void $ forkIO $ startGUI config $ setup tickEvent (optionTimers opts)

setup :: Event Tick -> [TimerSetup] -> Window -> UI ()
setup tickEvent timerSetups win = do
    UI.addStyleSheet win "semantic.min.css"
    timers <- mapM (setupTimer tickEvent) timerSetups
    void $ getBody win #+ [ View.centerGrid timers ]

setupTimer :: Event Tick -> TimerSetup -> UI Element
setupTimer tickEvent (TimerSetup time color) = do
    buttonStart <- View.buttonStart
    buttonReset <- View.buttonReset
    buttonGroup <- View.buttonGroup [ buttonStart, buttonReset ]

    displayText <- View.displayText ( showTimer time )
    display     <- View.display displayText

    content     <- View.content (toPct color 100) [ display, buttonGroup ]
    segment     <- View.segment content

    eActive <- accumE False $ unionWith const
        (not         <$ UI.click buttonStart)
        (const False <$ UI.click buttonReset)
    onEvent eActive $ \case
        True  -> pure buttonStart # set text "Stop"
        False -> pure buttonStart # set text "Start"

    bActive <- stepper False eActive
    eTimer <- accumE time $ unionWith const
        (const time  <$  UI.click buttonReset)
        (updateTimer <$> whenE bActive tickEvent)
    onEvent eTimer $ \timerState -> do
        pure displayText # set text (showTimer timerState)
        let pct = toPct color $ calcPct time timerState
        pure content # set style pct

    return segment

updateTimer :: Tick -> Timer -> Timer
updateTimer (Tick a) (Timer b) = Timer $ max 0 $ b - a

toPct :: Color -> Float -> [(String, String)]
toPct color pct = [("background" ,
    "linear-gradient(to right, " ++
        sRGB24show color ++ " " ++
        show pct ++ "%, white 0%)")]

calcPct :: Timer -> Timer -> Float
calcPct (Timer s) (Timer x) = realToFrac (100 * (x/s))

--------------------------------------------------------------------------------

newtype Tick = Tick NominalDiffTime

newTickEvent :: IO (Event Tick)
newTickEvent = do
    (tickEvent, fireTick) <- newEvent
    forkIO $ timeLoop fireTick =<< getCurrentTime
    return tickEvent
    where
    timeLoop fire last = do
        now <- getCurrentTime
        let diff = diffUTCTime now last
        fire $ Tick diff
        threadDelay 1000000
        timeLoop fire now

