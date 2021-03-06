{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileSystemMod
  ( MyFileSystem(..)
  , Directory(..)
  , File(..)
  , getDirInDir
  , getCurrentDirectory'
  , checkPath
  , showFileInfo
  , showDirectoryInfo
  , getCurrentPath
  , showDirContent
  , showCommit
  , getFileInDir
  , getDirectory
  , getFileName
  , changeDirectory
  , getDirectoryName
  , getFile
  , getPaths
  , getFileOrDirectory
  , getCvsDirInDir
  , getCvsDirName
  , getPrefix
  ) where

import Data.Time.Clock
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Control.Monad.Trans.Except
import Data.List(deleteBy)

-- | Data contains all information about current file system.
-- Init directory and folder names to current directory.
data MyFileSystem = MyFileSystem 
  { -- | init directory
    fsGetInitDir     :: Directory

    -- | array of folder names to current directory
  , fsGetCurrentPath :: [FilePath]
  }
  deriving(Show, Eq)

-- | Data contains needed information about directory:
-- subdirectories, files, cvs folders for all files if
-- they exists, absolute path and permissions.
data Directory = Directory 
  { dGetDirectories   :: [Directory] -- ^ all subdirectories
  , dGetFiles         :: [File]      -- ^ all files in this directory
  , dGetCvsDirs       :: [Directory] -- ^ all cvs-directories
  , dGetPath          :: FilePath    -- ^ absolute path to directory
  , dGetPermissions   :: Permissions -- ^ all directory permissions
  }
  deriving(Eq)

instance Show Directory where
  show d =
    "dPath: " ++ show (dGetPath d) ++ "  " ++
    "Dirs: " ++ show (dGetDirectories d) ++ "  " ++
    "Files: " ++ show (dGetFiles d) ++ "  " ++
    "CvsDirs: " ++ show (dGetCvsDirs d)

-- | Data file structure. Contains information about file:
-- absolute path, permissions, extention, size,
-- last modification time and content.
data File = File 
  { fGetPath        :: FilePath    -- ^ absolute path to file
  , fGetPermissions :: Permissions -- ^ permissions
  , fGetExtension   :: String      -- ^ extention (ex: ".txt")
  , fGetTime        :: UTCTime     -- ^ last modification time
  , fGetSize        :: Integer     -- ^ file size in bytes
  , fGetContent     :: String      -- ^ file content
  }
  deriving(Eq)

instance Show File where
  show f =
    "fPath: " ++ show (fGetPath f) ++ "  " ++
    "Cont: " ++ show (fGetContent f)

-- | Unique prefix for cvs folders.
getPrefix :: String
getPrefix = ".cvs_"
  
-- helpers -- 

checkPath :: Directory -> [FilePath] -> Bool
checkPath dir paths =
  case foldM getDirInDir dir paths of
    Just _ -> True
    _      -> False

changeDirectory :: Directory                -- current directory
                -> [FilePath]               -- path to needed directory
                -> (Directory               -- function for change directory
                -> Except String Directory)
                -> Except String Directory  -- changed directory
changeDirectory curDir [] func = func curDir
changeDirectory curDir (p : ps) func =
  let dirs = dGetDirectories curDir
      dir = getDirInDir curDir p
  in case dir of
    Just d ->
      case runExcept (changeDirectory d ps func) of
        Right newD -> return $ curDir {dGetDirectories = newD : deleteDir d dirs}
        Left err   -> throwE err
    Nothing -> throwE "Not found current directory?!"
  where
    eqDir x y = getDirectoryName x == getDirectoryName y
    deleteDir = deleteBy eqDir

-- getters --

showDirContent :: Directory -> String
showDirContent dir =
  let dirs   = dGetDirectories dir
      files  = dGetFiles dir
  in "Directories:\n" ++ unwords (map getDirectoryName dirs) ++ "\n" ++
     "Files:\n" ++ unwords (map getFileName files)

getCurrentDirectory' :: MyFileSystem -> Directory
getCurrentDirectory' MyFileSystem { fsGetInitDir = initDir
                                  , fsGetCurrentPath = curPath} =
  case getDirectory initDir curPath of
    Just dir -> dir
    Nothing  -> error "Current dir must be correct"

getDirectory :: Directory -> [FilePath] -> Maybe Directory
getDirectory = foldM getDirInDir

getFile :: Directory -> [FilePath] -> String -> Maybe File
getFile dir paths name = do
  curDir <- getDirectory dir paths
  getFileInDir curDir name

getFileOrDirectory :: Directory -> FilePath -> Maybe (Either File Directory)
getFileOrDirectory dir path = do
  let (partPathes, name) = getPaths path
  dir' <- getDirectory dir partPathes
  case getFile dir' [] name of
    Just f  -> return $ Left f
    Nothing -> do
      d <- getDirInDir dir' name
      return $ Right d

getDirInDir :: Directory -> String -> Maybe Directory
getDirInDir d@(Directory {dGetDirectories = dirs}) name =
  if null name
  then Just d
  else getDirInListOfDirs dirs name

getCvsDirInDir :: Directory -> String -> Maybe Directory
getCvsDirInDir Directory {dGetCvsDirs = dirs} = getDirInListOfDirs dirs

getDirInListOfDirs :: [Directory] -> String -> Maybe Directory
getDirInListOfDirs dirs name = 
  case filter ((== name) . getDirectoryName) dirs of
    [x] -> Just x
    _   -> Nothing

getFileInDir :: Directory -> String -> Maybe File
getFileInDir Directory {dGetFiles = files} name =
  case filter ((== name) . getFileName) files of
    [x] -> Just x
    _   -> Nothing

getCurrentPath :: MyFileSystem -> FilePath
getCurrentPath MyFileSystem { fsGetInitDir = dir
                             , fsGetCurrentPath = curPath
                             } =
  foldr (</>) "" ([dGetPath dir] ++ curPath)

getFileName :: File -> String
getFileName File {fGetPath = path} = takeFileName path

getDirectoryName :: Directory -> String
getDirectoryName Directory {dGetPath = path} = takeFileName path

getCvsDirName :: String -> String
getCvsDirName name = getPrefix ++ name

getPaths :: FilePath -> ([FilePath], FilePath)
getPaths path =
  let splitP = splitDirectories path in (init' splitP, takeFileName path)
  where
    init' :: [FilePath] -> [FilePath]
    init' ps = case ps of
                  [] -> []
                  _  -> init ps

-- show information --

showPermissions :: Permissions -> String
showPermissions perm = readSh ++ writeSh ++ execSh ++ searchSh
  where
    readSh =   if readable perm
               then "readable "
               else ""
    writeSh =  if writable perm
               then "writable "
               else ""
    execSh =   if executable perm
               then "executable "
               else ""
    searchSh = if searchable perm
               then "searchable "
               else ""

showFileInfo :: File -> String
showFileInfo file =
  "File path: " ++ fGetPath file ++ "\n" ++
  "Permissions: " ++ showPermissions (fGetPermissions file) ++ "\n" ++
  "Type: " ++ fGetExtension file ++ "\n" ++
  "Last update time: " ++ show (fGetTime file) ++ "\n" ++
  "Size: " ++ show (fGetSize file) ++ " bytes\n"

showDirectoryInfo :: Directory -> String
showDirectoryInfo dir =
  "Directory path: " ++ dGetPath dir ++ "\n" ++
  "Permissions: " ++ showPermissions (dGetPermissions dir) ++ "\n" ++
  "Files in dir: " ++ show (length (dGetFiles dir)) ++ "\n" ++
  "Size: " ++ show (getDirectorySize dir) ++ " bytes\n"

getDirectorySize :: Directory -> Integer
getDirectorySize Directory {dGetFiles = files, dGetDirectories = dirs} =
  filesSize + dirsSize
  where
    filesSize = sum $ map fGetSize files
    dirsSize  = sum $ map getDirectorySize dirs

showCommit :: Directory -> String
showCommit d = do
  let name = getDirectoryName d
  case getFileInDir d "commit.txt" of
    Just f -> name ++ ". " ++ fGetContent f
    Nothing -> error "commit.txt not found in cvs dir!!!!"
