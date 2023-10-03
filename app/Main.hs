{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Engine (KeyName (..)
              , Entity
              , Canvas
              , canvasToTex
              , makeCanvas
              , drawEntities
              , Physics (..))

import Engine.IO (
  initIO
  , getKeyPressed)
       
import Control.Monad (when)
import Prelude hiding (Left,Right)

main :: IO ()
main = do
  initIO
  let canvas = makeCanvas 20 80
  let et = makeEnemy 2 4
  let ss = makeSpaceShip 12 37
  controller [et] ss canvas


controller :: [Entity] -> Entity -> Canvas -> IO()
controller entities spaceship canvas = do
  act <- getKeyPressed

  let (_, h, w) = canvas
  let nss = actEntity spaceship (h, w) act
  let ncanvas = drawEntities (nss : entities) canvas
  putStr $ canvasToTex ncanvas
  controller entities nss canvas

actEntity :: Entity -> (Int, Int) -> KeyName -> Entity
actEntity (ed, dims, (y, x), p) _ Left = (ed, dims, (y, max 0 (x-1)), p)
actEntity (ed, (h, w), (y, x), p) (_, bw) Right = (ed, (h, w), (y, min (bw - w) (x+1)), p)
actEntity (ed, dims, (y, x), p) _ Up = (ed, dims, (max 0 (y-1), x), p)
actEntity (ed, (h, w), (y, x), p) (bh, _) Down = (ed, (h, w), (min (bh - h) (y+1), x), p)

actEntity e _ _ = e

makeEnemy :: Int -> Int -> Entity
makeEnemy y x =
  let o = 'o'
      z = ' ' in
  ([
      [z, o, z],
      [o, o, o],
      [z, o, z]
   ], (3, 3), (y, x), Physics {mass = 5, velocity = (0, 0), forces = [((1, 0), -1)]})

makeSpaceShip :: Int -> Int -> Entity
makeSpaceShip y x =
  let o = 'o'
      z = ' ' in
    ([
      [z, z, o, z, z],
      [z, o, o, o, z],
      [o, o, o, o, o]
     ], (3, 5), (y, x), Physics {mass = 5, velocity = (0, 0), forces = [((1, 0), -1)]})
  
