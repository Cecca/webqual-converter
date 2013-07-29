module Main where

import System.Environment (getArgs)
import Converter
import Data.ByteString.Lazy.Char8 as BSC hiding (map)

main::IO()
main = do
    args <- getArgs
    case args of
      [input,output] -> do
        inData <- BSC.readFile input
        let pairs = (processLines . BSC.lines) inData
        let outData = BSC.unlines $ map pairToStr pairs
        BSC.writeFile output outData
        print "Done"
      _ -> error "Please provide both input and output filenames"

