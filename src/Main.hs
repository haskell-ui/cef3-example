module Main where

import Graphics.UI.Threepenny.Core hiding (value)
import qualified Graphics.UI.Threepenny as UI

import Graphics.CEF3.Simple

import Options (Options(..), getOptions)

main :: IO ()
main = do
    opts <- getOptions
    startBrowserUrl $ "http://127.0.0.1:" ++ show (port opts)
