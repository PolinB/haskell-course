module BuildStructure
  ( getFileSystemIO
  , setDirectory
  ) where

import FileSystemMod

import Data.Time.Clock
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Data.List(isInfixOf)

getFileIO :: FilePath -> IO File
getFileIO path = do
  permission <- getPermissions path
  time       <- getModificationTime path
  size       <- getFileSize path
  content    <- readFile path
  return $ rememberFile path permission time size content

getDirectoryIO :: FilePath -> IO Directory
getDirectoryIO path = do
  permission <- getPermissions path
  arr <- listDirectory path
  let absArr = map ((</>) path) arr
  filesPath <- filterM doesFileExist absArr
  dirsPath <- filterM doesDirectoryExist absArr
  let simpleDirs = filter (not . isInfixOf ".cvs_" . takeFileName) dirsPath
  let cvsDirs = filter (isInfixOf ".cvs_" . takeFileName) dirsPath
  files <- mapM getFileIO filesPath
  if null dirsPath
  then return $ Directory { dGetDirectories = []
                          , dGetFiles       = files
                          , dGetCvsDirs     = []
                          , dGetPath        = path
                          , dGetPermissions = permission
                          }
  else do
    dirs <- mapM getDirectoryIO simpleDirs
    cvs <- mapM getDirectoryIO cvsDirs
    return $ Directory { dGetDirectories = dirs
                       , dGetFiles       = files
                       , dGetCvsDirs     = cvs
                       , dGetPath        = path
                       , dGetPermissions = permission
                       }

-- | Get information about needed directory and return fyle system
-- with all inf about files and subdirectories.
getFileSystemIO :: FilePath        -- ^ path to init directory
                -> IO MyFileSystem -- ^ file system
getFileSystemIO path = do
  dir <- getDirectoryIO path
  return $ MyFileSystem { fsGetInitDir     = dir
                        , fsGetCurrentPath = []
                        }

rememberFile :: FilePath -> Permissions -> UTCTime -> Integer -> String -> File
rememberFile path perm time size content = 
  File { fGetPath = path
       , fGetPermissions = perm
       , fGetExtension = takeExtension path
       , fGetTime = time
       , fGetSize = size
       , fGetContent = content
       }

setFile :: File -> IO ()
setFile file = do
  let path = fGetPath file
  let content = fGetContent file
  let perm = fGetPermissions file
  let time = fGetTime file
  _ <- writeFile path content
  _ <- setPermissions path perm
  _ <- setModificationTime path time
  return ()

-- | Save all directory (with files and subdirectories) on
-- real file system.
setDirectory :: Directory -> IO ()
setDirectory dir = do
  let path = dGetPath dir
  _ <- createDirectory path
  let perm = dGetPermissions dir
  _ <- setPermissions path perm
  let dirs = dGetDirectories dir
  mapM_ setDirectory dirs
  let cvsDirs = dGetCvsDirs dir
  mapM_ setDirectory cvsDirs 
  let files = dGetFiles dir
  mapM_ setFile files
