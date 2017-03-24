{-# Language NoImplicitPrelude #-}
{-# Language LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Protolude
import Prelude (String)
import Text.Printf
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import System.Environment
import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI
import Graphics.CEF3.Simple

import Options (Options(..), getOptions)
import qualified View

--------------------------------------------------------------------------------

main :: IO ()
main = handleSubProcess $ do
    opts <- getOptions
    startCountDown opts
    startBrowserUrl $ "http://127.0.0.1:" ++ show (port opts)

startCountDown :: Options -> IO ()
startCountDown opts = do
    ep <- getExecutablePath
    let sloc = ep ++ "/static"

    let config = defaultConfig
            { jsPort   = Just $ port opts
            , jsStatic = Just sloc }
    eventTime <- timer
    forkIO $ startGUI config $ setup eventTime
    threadDelay 5000

setup :: Event Timer -> Window -> UI ()
setup eventTime win = do
    UI.addStyleSheet win "semantic.min.css"

    let timer1Start = mkTimer 3  0 0
    let timer2Start = mkTimer 1 30 0

    timer1 <- setupTimer "#00b5ad" timer1Start eventTime
    timer2 <- setupTimer "#e67b9e" timer2Start eventTime

    void $ getBody win #+ [ View.centerGrid [timer1, timer2] ]

type Color = String
setupTimer :: Color -> Timer -> Event Timer -> UI Element
setupTimer color timerStart eventTime = do
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

    let eReset  = Reset timerStart <$ UI.click buttonReset
    let eRun    = Run <$> eActiveTimer
    let eAction = unionWith const eReset eRun
    eTimer <- accumE timerStart (setTimer <$> eAction)

    displayText <- UI.h1 # set text (showTimer timerStart)
    display <- View.display #+ [ element displayText ]

    content <- View.content
        # set children [ display, buttonGroup ]
        # set style (toPct color 100)
    segment <- View.segment
        # set children [ content ]

    onEvent eTimer $ \timerState -> do
        pure displayText # set text (showTimer timerState)
        let pct = toPct color $ calcPct timerStart timerState
        pure content # set style pct

    return segment

data TimerAction = Reset Timer | Run Timer
setTimer :: TimerAction -> Timer -> Timer
setTimer (Reset t)       _         = t
setTimer (Run (Timer a)) (Timer b) = Timer $ max 0 $ b - a

toPct :: Color -> Float -> [(String, String)]
toPct color pct = [("background" ,
    "linear-gradient(to right, " ++
        color ++ " " ++
        show pct ++ "%, white 0%)")]

calcPct :: Timer -> Timer -> Float
calcPct (Timer s) (Timer x) = realToFrac (100 * (x/s))

--------------------------------------------------------------------------------

newtype Timer = Timer NominalDiffTime

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

showTimer :: Timer -> String
showTimer (Timer d) = printf "%02d:%02d:%02d" hs ms ss
    where
    s  = floor d :: Int
    hs = div s (60*60)
    ms = div (s - hs*60*60) 60
    ss = s - (ms*60 + hs*60*60)

mkTimer :: Int -> Int -> Int -> Timer
mkTimer hs ms ss = Timer $ fromIntegral $ h*60*60 + m*60 + s
    where
    h = max 0 hs
    m = max 0 $ min 59 ms
    s = max 0 $ min 59 ss


