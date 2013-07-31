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
import qualified Control.Monad.Parallel as MP (mapM)
import System.Exit


main :: IO()
main = do
    args <- getArgs
    case args of
      ["-h"] -> putStrLn help
      [opType,input,output] -> do
        let conversionFunc = case opType of
              "-u" -> convertUrlsFile
              "-l" -> convertLinksFile
              _ -> error "Unknown operation"
        createDirectory output
        inputFiles <- getFiles input
        MP.mapM (convertWith conversionFunc input output) inputFiles
        putStrLn "Done!"
      _ -> putStrLn help >> exitFailure

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


help :: String
help = "webqual-converter: converts datasets from one format to another\n\n" ++
       "USAGE:\n" ++
       "    webual-converter [options] input output\n\n" ++
       "where the available options are the following:\n\n" ++
       "    -u  :  convert url files. The input should be a set of url\n" ++
       "           files, with a url for each line. The output is a\n" ++
       "           set of files with a url per line together with their\n" ++
       "           64 bit hashes.\n" ++
       "    -l  :  convert links files. The input is a set of binary files\n" ++
       "           representing adjacency lists with 128-bits IDs.\n" ++
       "           the output is a set of binary files containing adjacency\n"++
       "           lists with 64 bits IDs.\n" ++
       "    -h  :  prints this help and exits.\n\n" ++
       "Note that input and output parameters must be directories\n" ++
       "the files to be converted.\n\n" ++
       "The program can take advantage of multiple cores by using each\n" ++
       "core to process a file. In order to use this feature you should\n" ++
       "invoke the program like the following:\n\n" ++
       "    webqual-converter [options] input output +RTS -N4\n\n" ++
       "if you have, for instance, 4 cores.\n"

