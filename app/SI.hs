module SI where
import Engine (KeyName (..)
              , Entity
              , entityCoord
              , Canvas
              , makeCanvas
              , Physics (..)
              , gameTick)

import Prelude hiding (Left,Right)

createGame :: Game ()
createGame = Game { _canvas = makeCanvas 30 100
                  , _entities = []
                  , _ss = makeSpaceShip 20 40
                  , _end = False
                  }

isEnemy :: Entity -> Bool
isEnemy (_, (2, 1), _, _) = False
isEnemy (_, (3, 5), _, _) = False
isEnemy _ = True

countEnemies :: [Entity] -> Int
countEnemies (e : es) = if isEnemy e then 1 + countEnemies es else countEnemies es
countEnemies [] = 0

collide' :: Entity -> Entity -> Bool
collide' (_, (h1, w1), (y1, x1), _) (_, _, (y2, x2), _) = (x2 >= x1 + w1) || (y2 >= y1 + h1)

collide :: Entity -> Entity -> Bool
collide e1 e2 = not (collide' e1 e2 || collide' e2 e1)

collision :: [Entity] -> [Entity]
collision [] = []
collision (e1 : es) = nes
  where
    hit = any (collide e1) es
    rest = filter (not . collide e1) es
    nes = if hit then collision rest else e1 : collision es


inbound :: (Int, Int) -> [Entity] -> [Entity]
inbound (bh, bw) es = [e | e <- es, fst (entityCoord e) < bh ]

makeWave (bh, bw) = take 20 [makeEnemy ((i `div` bw) * 4 ) (i `mod` bw) | i <- [0, 4..]]

step :: (Int, Int) -> Entity -> [Entity] -> [Entity]
step bdims ss es = nes
  where
    inb = inbound bdims es
    nes' = gameTick inb
    nes'' = [e | e <- collision nes', not (collide ss e)]
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
     ], (2,1), (y - 2, x + (w `div` 2)), Physics {mass = 2, velocity = (-3, 0), forces = [((0.2, 0), -1)]})

data Game n = Game
  {
    _canvas :: Canvas
  , _entities :: [Entity]
  , _ss :: Entity
  , _end :: Bool
  }
