module Main where
import System.IO(stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
import Control.Monad (when)
import Prelude hiding (Right, Left)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  let board = makeBoard 20 80
  let et = makeEnemy 2 4
  let ss = makeSpaceShip 12 37
  controller [et] ss board

findAction :: [Char] -> Action
findAction inkey = case inkey of
      "\ESC[A" -> Up
      "\ESC[B" -> Down
      "\ESC[C" -> Right
      "\ESC[D" -> Left
      " "      -> Fire
      _        -> Skip

controller :: [Entity] -> Entity -> Board -> IO()
controller entities spaceship board = do
  key <- getKey
  let act = findAction key

  let (_, h, w) = board
  let nss = actEntity spaceship (h, w) act
  let nboard = drawEntities (nss : entities) board
  putStr $ boardToTex nboard
  controller entities nss board
  
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)
          

data Action = Left | Right | Up | Down | Fire | Skip deriving (Enum)
type Board = ([[Char]], Int, Int)
type Entity = ([[Char]], (Int,Int), (Int, Int))

drawEntities :: [Entity] -> Board -> Board
drawEntities [] board = board
drawEntities (e:es) board = drawEntities es $ drawEntityHelper board e

actEntity :: Entity -> (Int, Int) -> Action -> Entity
actEntity (ed, dims, (y, x)) _ Left = (ed, dims, (y, max 0 (x-1)))
actEntity (ed, (h, w), (y, x)) (_, bw) Right = (ed, (h, w), (y, min (bw - w) (x+1)))
actEntity (ed, dims, (y, x)) _ Up = (ed, dims, (max 0 (y-1), x))
actEntity (ed, (h, w), (y, x)) (bh, _) Down = (ed, (h, w), (min (bh - h) (y+1), x))

actEntity e _ _ = e

makeEnemy y x =
  let o = 'o'
      z = ' ' in
  ([
      [z, o, z],
      [o, o, o],
      [z, o, z]
   ], (3, 3), (y, x))

makeSpaceShip y x=
  let o = 'o'
      z = ' ' in
    ([
      [z, z, o, z, z],
      [z, o, o, o, z],
      [o, o, o, o, o]
     ], (3, 5), (y, x))
  
removeColumn :: [[a]] -> ([a], [[a]])
removeColumn [] = ([], [])
removeColumn ((c:b):bs) = let (cs, rs) = removeColumn bs in ((c:cs), (b:rs))

  
addColumn (c:cs) (r:rs) = (c:r) : addColumn cs rs
addColumn _ rs = rs

drawEntityHelper :: Board -> Entity -> Board
drawEntityHelper (board, h, w) (e, dims, (0, 0)) = (drawEntityHere board e, h, w)
drawEntityHelper (board, h, w) (e, dims, (0, x)) = (addColumn bcol b2, h, w)
  where
    (bcol, bgrid) = removeColumn board
    (b2, _, _) = drawEntityHelper ((bgrid), h, w-1) (e, dims, (0, x - 1))
    
drawEntityHelper ((ccs:bs), h, w) (e, dims, (y, 0)) = let (nbs, _, _) = drawEntityHelper (bs, h-1, w) (e, dims, (y - 1, 0))
  in (ccs : nbs, h, w)
drawEntityHelper ((ccs:bs), h, w) (e, dims, (y, x)) = let (nbs, _, _) = drawEntityHelper (bs, h-1, w) (e, dims, (y - 1, x))
  in (ccs : nbs, h, w)
  
drawEntityHelper board e = board

drawEntityHere (cs:bs) (es:rs) = (drawEntityRow cs es) : (drawEntityHere bs rs)
drawEntityHere bs _ = bs
  
drawEntityRow (c:cs) (e:es) = e : drawEntityRow cs es
drawEntityRow c _ = c

makeBoard h w = ([[' ' | x <- [0..w]] | y <- [0..h]], h, w)
boardToTex :: Board -> String
boardToTex (b, h, n) = 
  "[" ++ ['=' | x <- [0..n]] ++  "]\n"
  ++ boardToTexHelper (b, h, n)
  
boardToTexHelper ([], _, n) = "[" ++ ['=' | x <- [0..n]] ++ "]\n"
boardToTexHelper (b:bs, h, w) = "[" ++ boardRow b ++ "]\n" ++ boardToTexHelper (bs, h, w)
  
boardRow [] = ""
boardRow (b:bs) = [b] ++  boardRow bs
