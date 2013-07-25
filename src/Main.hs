module Main where

{-import Data.ByteString as BSS (unpack)-}
{-import Data.ByteString.Lazy as BS-}
{-import Data.ByteString.Lazy.Char8 as BSC-}
import System.Environment (getArgs)
import Converter
{-import Crypto.Hash-}
{-import Text.Printf-}


main::IO()
main = do
    args <- getArgs
    case args of
      [input,output] -> do
        inData <- readFile input
        let pairs = (processLines . lines) inData
        let outData = unlines $ map pairToStr pairs
        writeFile output outData
        print "Done"
      _ -> error "Please provide both input and output filenames"
