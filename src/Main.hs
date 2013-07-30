module Main where

import System.Environment (getArgs)
import Converter
import Data.ByteString.Lazy.Char8 as BSC hiding (map, putStrLn, filter)
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad


main :: IO()
main = do
    args <- getArgs
    case args of
      ["-u",input,output] -> do
          convertUrlsFile input output
          putStrLn $ "Converted " ++ input
      ["-l",input,output] -> do
          convertLinksFile input output
          putStrLn $ "Converted " ++ input
      _ -> error "Please provide both input and output filenames"


convertUrlsFile :: FilePath -> FilePath -> IO ()
convertUrlsFile input output = do
    inData <- BSC.readFile input
    let pairs = (processLines . BSC.lines) inData
    let outData = BSC.unlines $ map pairToStr pairs
    BSC.writeFile output outData

convertLinksFile :: FilePath -> FilePath -> IO ()
convertLinksFile input output = do
    inData <- BSC.readFile input
    let origHashes = groupHashes inData
    let smallHashes = map get64bitHash origHashes
    BSC.writeFile output $ unGroupHashes smallHashes

getFiles :: FilePath -> IO [FilePath]
getFiles dir =
  getDirectoryContents dir >>=
  filterM doesFileExist >>=
  \paths -> return $ map (dir </>) paths


