module Main where

import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI

import Graphics.CEF3.Simple

main :: IO ()
main = do
    startBrowserUrl "http://127.0.0.1:8099"
