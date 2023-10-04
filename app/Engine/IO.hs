module Engine.IO
  ( initIO
  , getKeyPressed )
where

import Engine ( KeyName (Up, Down, Right, Left, Space, Skip))
import Prelude hiding (Left, Right)
import System.IO(stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))

initIO :: IO ()
initIO = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

getKeyPressed :: IO KeyName
getKeyPressed = do
  key <- getKey
  return $ findKeyName key

  
findKeyName :: [Char] -> KeyName
findKeyName inkey = case inkey of
      "\ESC[A" -> Up
      "\ESC[B" -> Down
      "\ESC[C" -> Right
      "\ESC[D" -> Left
      " "      -> Space
      _        -> Skip
  
getKey :: IO [Char]
getKey = do
  avail <- hReady stdin
  reverse <$> getKey' avail ""
  where getKey' True chars = do
          char <- getChar
          more <- hReady stdin
          getKey' more (char:chars)
        getKey' False chars = return chars
  
