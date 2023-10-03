module Engine
  ( KeyName (Up, Down, Right, Left, Space, Skip)
  , Canvas
  , Entity
  , drawEntities
  , canvasToTex
  , makeCanvas
  )
where


data KeyName = Left | Right | Up | Down | Space | Skip deriving (Enum)
type Canvas = ([[Char]], Int, Int)
type Entity = ([[Char]], (Int,Int), (Int, Int))

drawEntities :: [Entity] -> Canvas -> Canvas
drawEntities (e:es) canvas = drawEntities es $ drawEntityHelper canvas e
drawEntities _ canvas = canvas

removeColumn :: [[a]] -> ([a], [[a]])
removeColumn [] = ([], [])
removeColumn ((c:b):bs) = let (cs, rs) = removeColumn bs in (c:cs, b:rs)

  
addColumn (c:cs) (r:rs) = (c:r) : addColumn cs rs
addColumn _ rs = rs

drawEntityHelper :: Canvas -> Entity -> Canvas
drawEntityHelper (canvas, h, w) (e, dims, (0, 0)) = (drawEntityHere canvas e, h, w)
drawEntityHelper (canvas, h, w) (e, dims, (0, x)) = (addColumn bcol b2, h, w)
  where
    (bcol, bgrid) = removeColumn canvas
    (b2, _, _) = drawEntityHelper (bgrid, h, w-1) (e, dims, (0, x - 1))
    
drawEntityHelper (ccs:bs, h, w) (e, dims, (y, 0)) = let (nbs, _, _) = drawEntityHelper (bs, h-1, w) (e, dims, (y - 1, 0))
  in (ccs : nbs, h, w)
drawEntityHelper (ccs:bs, h, w) (e, dims, (y, x)) = let (nbs, _, _) = drawEntityHelper (bs, h-1, w) (e, dims, (y - 1, x))
  in (ccs : nbs, h, w)
  
drawEntityHelper canvas e = canvas

drawEntityHere (cs:bs) (es:rs) = drawEntityRow cs es : drawEntityHere bs rs
drawEntityHere bs _ = bs
  
drawEntityRow (c:cs) (e:es) = e : drawEntityRow cs es
drawEntityRow c _ = c

makeCanvas h w = ([[' ' | x <- [0..w]] | y <- [0..h]], h, w)
canvasToTex :: Canvas -> String
canvasToTex (b, h, n) = 
  "[" ++ ['=' | x <- [0..n]] ++  "]\n"
  ++ canvasToTexHelper (b, h, n)
  
canvasToTexHelper ([], _, n) = "[" ++ ['=' | x <- [0..n]] ++ "]\n"
canvasToTexHelper (b:bs, h, w) = "[" ++ canvasRow b ++ "]\n" ++ canvasToTexHelper (bs, h, w) 
  
canvasRow (b:bs) = b :  canvasRow bs
canvasRow _ = ""
