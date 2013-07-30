module Main where

import System.Environment (getArgs)
import Converter
import Data.ByteString.Lazy.Char8 as BSC hiding (map, putStrLn, filter)
import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , createDirectory
                        , getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad


main :: IO()
main = do
    args <- getArgs
    case args of
      [opType,input,output] -> do
        let conversionFunc = case opType of
              "-u" -> convertUrlsFile
              "-l" -> convertLinksFile
              _ -> error "Unknown operation"
        createDirectory output
        inputFiles <- getFiles input
        mapM_ (convertWith conversionFunc input output) inputFiles
        putStrLn "Done!"
      _ -> error "Please provide both input and output filenames"

convertWith :: (FilePath -> FilePath -> IO())
            -> FilePath
            -> FilePath
            -> FilePath
            -> IO ()
convertWith func inDir outDir file = do
  func (inDir </> file) (outDir </> file)
  putStrLn $ "Converted " ++ file

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
getFiles dir = getDirectoryContents dir >>= filterM doesFileExist


