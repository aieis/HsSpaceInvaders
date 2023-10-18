module SI where
import Engine (KeyName (..)
              , Entity
              , entityCoord
              , Canvas
              , canvasToTex
              , makeCanvas
              , drawEntities
              , Physics (..)
              , gameTick)

import Engine.IO (
  initIO
  , getKeyPressed)

import Control.Monad (when, unless)
import Prelude hiding (Left,Right)
import System.Clock (Clock(Monotonic), TimeSpec, getTime, toNanoSecs, diffTimeSpec)
import Control.Concurrent (threadDelay)

mainOld :: IO ()
mainOld = do
  initIO
  let canvas = makeCanvas 50 50
  let et = makeEnemy 2 4
  let ss = makeSpaceShip 45 37
  start <- getTime Monotonic
  controller start [et] ss canvas

createGame :: Game ()
createGame = Game { _canvas = makeCanvas 30 100
                  , _entities = []
                  , _ss = makeSpaceShip 20 40
                  , _end = False
                  }

idIO :: Int -> IO ()
idIO _ = do
  return ()

controller :: TimeSpec -> [Entity] -> Entity -> Canvas -> IO()
controller start entities spaceship canvas = do
  act <- getKeyPressed
  let (_, h, w) = canvas
  let (nss : bullets) = actEntity spaceship (h, w) act

  let nents = gameTick (bullets ++ entities)
  let ncanvas = drawEntities (nss : nents) canvas
  let tex = canvasToTex ncanvas

  end <- getTime Monotonic
  let diff = fromIntegral (toNanoSecs  (diffTimeSpec end start) `div` 1000)
  (if diff > 100000 then idIO else threadDelay ) diff
  putStr $ canvasToTex ncanvas

  controller end nents nss canvas


isEnemy :: Entity -> Bool
isEnemy (_, (2, 1), _, _) = False
isEnemy (_, (3, 5), _, _) = False
isEnemy _ = True

countEnemies :: [Entity] -> Int
countEnemies (e : es) = if isEnemy e then 1 + countEnemies es else countEnemies es
countEnemies [] = 0

collide' :: Entity -> Entity -> Bool
collide' (_, (h1, w1), (y1, x1), _) (_, (h2, w2), (y2, x2), _) = (x2 > x1 + w2) || (y2 > y1 + h1)

collide :: Entity -> Entity -> Bool
collide e1 e2 = not (collide' e1 e2 || collide' e2 e1)

collision' (e1 : e2 : es) = if (collide e1 e2) then es else e2 : collision (e1 : es)
collision' es = es

collision :: [Entity] -> [Entity]
collision [] = []
collision ei@(e1 : es) = nes
  where
    nes' = collision' ei
    n = length ei
    nes = if n == length nes' then e1 : collision (take (n - 1) nes') else collision nes'


inbound :: (Int, Int) -> [Entity] -> [Entity]
inbound (bh, bw) es = [e | e <- es, fst (entityCoord e) < bh ]

makeWave (bh, bw) = take 1 [makeEnemy ((i `div` bw) * 4 ) (i `mod` bw) | i <- [0, 4..]]

step :: (Int, Int) -> [Entity] -> [Entity]
step bdims es = nes
  where
    inb = inbound bdims es
    nes' = gameTick inb
    nes'' = collision nes'
    nwave = if countEnemies nes'' == 0 then makeWave bdims else []

    nes = nes'' ++ nwave
    

actEntity :: Entity -> (Int, Int) -> KeyName -> [Entity]
actEntity (ed, dims, (y, x), p) _ Left = [(ed, dims, (y, max 0 (x-1)), p)]
actEntity (ed, (h, w), (y, x), p) (_, bw) Right = [(ed, (h, w), (y, min (bw - w) (x+1)), p)]
actEntity (ed, dims, (y, x), p) _ Up = [(ed, dims, (max 0 (y-1), x), p)]
actEntity (ed, (h, w), (y, x), p) (bh, _) Down = [(ed, (h, w), (min (bh - h) (y+1), x), p)]
actEntity e1 _ Space = [e1, makeBullet e1]

actEntity e _ _ = [e]

makeEnemy :: Int -> Int -> Entity
makeEnemy y x =
  let o = 'o'
      z = ' ' in
  ([
      [z, o, z],
      [o, o, o],
      [z, o, z]
   ], (3, 3), (y, x), Physics {mass = 5, velocity = (0, 0), forces = []})

makeSpaceShip :: Int -> Int -> Entity
makeSpaceShip y x =
  let o = 'o'
      z = ' ' in
    ([
      [z, z, o, z, z],
      [z, o, o, o, z],
      [o, o, o, o, o]
     ], (3, 5), (y, x), Physics {mass = 5, velocity = (0, 0), forces = []})

makeBullet :: Entity -> Entity
makeBullet (_, (h, w), (y,x), p) =   let o = 'o' in
    ([[o],
      [o]
     ], (2,1), (y - 2, x + (w `div` 2)), Physics {mass = 2, velocity = (-6, 0), forces = [((1, 0), -1)]})

data Game n = Game
  {
    _canvas :: Canvas
  , _entities :: [Entity]
  , _ss :: Entity
  , _end :: Bool
  }
