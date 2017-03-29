{-# Language NoImplicitPrelude #-}
{-# Language LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Protolude
import Prelude (String)
import Data.Time (getCurrentTime, diffUTCTime)
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
    startCountDown opts
    startBrowserUrl $ "http://127.0.0.1:" ++ show (optionPort opts)

startCountDown :: Options -> IO ()
startCountDown opts = do
    ep <- dropFileName <$> getExecutablePath
    let config = defaultConfig
            { jsPort   = Just $ optionPort opts
            , jsStatic = Just $ ep ++ "/static"  }
    eventTime <- timer
    void $ forkIO $ startGUI config $ setup eventTime (optionTimers opts)
    threadDelay 5000

setup :: Event Timer -> [TimerSetup] -> Window -> UI ()
setup eventTime timerSetups win = do
    UI.addStyleSheet win "semantic.min.css"
    timers <- mapM (setupTimer eventTime) timerSetups
    void $ getBody win #+ [ View.centerGrid timers ]

setupTimer :: Event Timer -> TimerSetup -> UI Element
setupTimer eventTime (TimerSetup time color) = do
    buttonStop  <- View.buttonStop
    buttonReset <- View.buttonReset
    buttonGroup <- View.buttonGroup [ buttonStop, buttonReset ]

    let eStopOrReset = unionWith const
            (const False <$ UI.click buttonReset)
            (not         <$ UI.click buttonStop)
    eActive <- accumE False eStopOrReset
    onEvent eActive $ \case
        False -> pure buttonStop # set text "Start"
        True  -> pure buttonStop # set text "Stop"
    bStop <- stepper False eActive
    let eActiveTimer = whenE bStop eventTime

    let eReset  = Reset time <$ UI.click buttonReset
    let eRun    = Run <$> eActiveTimer
    let eAction = unionWith const eReset eRun
    eTimer <- accumE time (setTimer <$> eAction)

    displayText <- UI.h1 # set text (showTimer time)
    display <- View.display #+ [ element displayText ]

    content <- View.content
        # set children [ display, buttonGroup ]
        # set style (toPct color 100)
    segment <- View.segment
        # set children [ content ]

    onEvent eTimer $ \timerState -> do
        pure displayText # set text (showTimer timerState)
        let pct = toPct color $ calcPct time timerState
        pure content # set style pct

    return segment

data TimerAction = Reset Timer | Run Timer
setTimer :: TimerAction -> Timer -> Timer
setTimer (Reset t)       _         = t
setTimer (Run (Timer a)) (Timer b) = Timer $ max 0 $ b - a

toPct :: Color -> Float -> [(String, String)]
toPct color pct = [("background" ,
    "linear-gradient(to right, " ++
        sRGB24show color ++ " " ++
        show pct ++ "%, white 0%)")]

calcPct :: Timer -> Timer -> Float
calcPct (Timer s) (Timer x) = realToFrac (100 * (x/s))

--------------------------------------------------------------------------------

timer :: IO (Event Timer)
timer = do
    (eventTime, fireTime) <- newEvent
    forkIO $ timeLoop fireTime =<< getCurrentTime
    return eventTime
    where
    timeLoop fire last = do
        now <- getCurrentTime
        let diff = diffUTCTime now last
        fire $ Timer diff
        threadDelay 1000000
        timeLoop fire now

