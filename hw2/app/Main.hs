module Main where

import FileSystemMod
import BuildStructure
import InteractiveIO

import System.Directory
import System.Environment

-- args : "absolute path to init directory"
main :: IO ()
main = do
  args <- getArgs :: IO [String]
  case args of
    [dirPath] -> do
      fs  <- getFileSystemIO dirPath
      fs' <- interactive fs
      _   <- removeDirectoryRecursive dirPath
      _   <- setDirectory $ fsGetInitDir fs'
      return ()
    _ -> putStrLn "Add one directory path args, please"
