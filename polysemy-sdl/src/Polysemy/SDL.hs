{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Polysemy.SDL (
    SDL(..)
  , runSDLasSDL
  , WindowConfig, Window, Surface
  , V4(..), Rectangle(..)
  
  , initializeAll
  , createWindow
  , getWindowSurface
  , surfaceFillRect
  , updateWindowSurface
  , delay
  , destroyWindow
  , quit
  
  , defaultWindow
  ) where

import           Polysemy          (Member, Sem, Embed)
import qualified Polysemy          as P
import qualified Polysemy.Internal as P

import           SDL       (WindowConfig, Window, Surface)
import           SDL       (V4, Rectangle)
import qualified SDL as S

import           Data.Text (Text)
import           Data.Word (Word8, Word32)
import           Foreign.C.Types (CInt)

data SDL m a where
  InitializeAll :: SDL m ()
  CreateWindow :: Text -> WindowConfig -> SDL m Window
  GetWindowSurface :: Window -> SDL m Surface
  SurfaceFillRect :: Surface -> Maybe (S.Rectangle CInt) -> V4 Word8 -> SDL m ()
  UpdateWindowSurface :: Window -> SDL m ()
  Delay :: Word32 -> SDL m ()
  DestroyWindow :: Window -> SDL m ()
  Quit :: SDL m ()
  
P.makeSem ''SDL
  
runSDLasSDL :: Member (Embed IO) r => Sem (SDL ': r) a -> Sem r a
runSDLasSDL = P.interpret $ \case
  InitializeAll -> S.initializeAll
  CreateWindow t wc -> S.createWindow t wc
  GetWindowSurface w -> S.getWindowSurface w
  SurfaceFillRect s r c -> S.surfaceFillRect s r c
  UpdateWindowSurface w -> S.updateWindowSurface w
  Delay t -> S.delay t
  DestroyWindow w -> S.destroyWindow w
  Quit -> S.quit
  
defaultWindow = S.defaultWindow

