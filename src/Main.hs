module Main where

import System.Environment (getArgs)
import Converter
import Data.ByteString.Lazy.Char8 as BSC hiding (map)

main::IO()
main = do
    args <- getArgs
    case args of
      ["-u",input,output] -> do
          inData <- BSC.readFile input
          let pairs = (processLines . BSC.lines) inData
          let outData = BSC.unlines $ map pairToStr pairs
          BSC.writeFile output outData
          print "Done"
      ["-l",input,output] -> do
          inData <- BSC.readFile input
          let origHashes = groupHashes inData
          let smallHashes = map get64bitHash origHashes
          BSC.writeFile output $ unGroupHashes smallHashes
          print "Done"
      _ -> error "Please provide both input and output filenames"

