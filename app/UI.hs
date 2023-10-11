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
handleEvent (T.VtyEvent e) = do
  (_, bh, bw) <- use canvas
  nss <- use ss
  let act k ss = actEntity ss (bh, bw) k
  let entAct k ents = ents ++ tail (actEntity nss (bh, bw) k)
  let ind i es = es !! i
  case e of
       V.EvKey (V.KChar 'w') [] -> do
         ss %= (ind 0) . (act Up)
         entities %= entAct Up
       V.EvKey (V.KChar 'a') [] -> do
         ss %= (ind 0) . (act Left)
         entities %= entAct Left
       V.EvKey (V.KChar 's') [] -> do
         ss %= (ind 0) . (act Down)
         entities %= entAct Down
       V.EvKey (V.KChar 'd') [] -> do
         ss %= (ind 0) . (act Right)
         entities %= entAct Right
       V.EvKey (V.KChar ' ') [] -> do
         ss %= (ind 0) . (act Space)
         entities %= entAct Space
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
