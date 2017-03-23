{-# Language NoImplicitPrelude #-}
module Main where

import Protolude
import System.Environment
import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI
import Graphics.CEF3.Simple
import Options (Options(..), getOptions)

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
    void $ forkIO $ startGUI config setup
    threadDelay 5000

setup :: Window -> UI ()
setup win = do
    UI.addStyleSheet win "semantic.min.css"
    void $ getBody win #+ [ UI.h1 # set text "Test" ]
