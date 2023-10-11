{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import SI
import Engine
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Linear.V2 (V2(..))
import Lens.Micro.Mtl
import Lens.Micro.TH

import Prelude hiding (Left, Right)

makeLenses ''Game

data Tick = Tick

type Name = ()

app :: App (Game ()) Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

theMap :: AttrMap
theMap = attrMap V.defAttr
  []
  

handleEvent :: BrickEvent n e -> EventM n (Game ()) ()
handleEvent (T.VtyEvent e) =
  let act k ss = actEntity ss (20, 40) k
      ind0 k = k !! 0
  in case e of
       V.EvKey (V.KChar 'w') [] -> ss %= ind0 . (act Up)
       V.EvKey (V.KChar 'a') [] -> ss %= ind0 . (act Left)
       V.EvKey (V.KChar 's') [] -> ss %= ind0 . (act Down)
       V.EvKey (V.KChar 'd') [] -> ss %= ind0 . (act Right)
       _ -> return ()     

handleEvent _ = return ()


drawUI :: (Game ()) -> [Widget Name]
drawUI g = [str tex] where
  ncanvas = drawEntities (_ss g : _entities g) (_canvas g)
  tex = canvasToTex ncanvas
  
main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let g = createGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g
