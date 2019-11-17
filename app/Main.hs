{-# LANGUAGE OverloadedStrings #-}


module Main where

import Lib
import System.Environment (getArgs)
import Data.Function ((&))
import qualified Polysemy.SDL as PS
import qualified Polysemy as P

main :: IO ()
main = do
  args <- getArgs
  (program args) & PS.runSDLasSDL & P.runM
  
program :: forall r. P.Members '[PS.SDL] r => [String] -> P.Sem r ()
program args = do
  PS.initializeAll
  window <- PS.createWindow "SDL Tutorial" (PS.defaultWindow)
  surface <- PS.getWindowSurface window
  PS.surfaceFillRect surface Nothing (PS.V4 255 255 255 255)
  PS.updateWindowSurface window
  PS.delay 2000
  PS.destroyWindow window
  PS.quit
