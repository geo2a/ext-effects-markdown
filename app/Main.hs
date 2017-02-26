module Main where

import System.Environment
import System.IO
import Control.Monad

import Parsers
import MDParse
import HTMLGen

data OutputFormat = HTML

-- outputName :: OutputFormat -> String
-- outputName HTML = "output.html"

errorLogFile :: String
errorLogFile = "error.log"

main = do
  [fname, "-o", outname] <- getArgs
  raw <- readFile fname
  -- putStrLn $ "\nGenerated html, will be written to " ++ outname ++ ": "
  print $ parse doc raw
  case (parse doc raw) of
    (rest, Just tree) -> do
      --putStrLn $ "Parsed markdown: " ++ show(tree)
      let html = generateHTML outname $ tree
      --print $ html
      writeFile outname html
    (_, Nothing) ->
      return ()
      -- writeFile (show err) errorLogFile