module Commands
  ( cd
  , dirCommand
  , lsCommand
  , createFolder
  , createFile
  , catCommand
  , writeFileCommand
  , infoFD
  , findFileCommand
  , remCommand
  , helpCommand
  , initCommand
  , addCvsCommand
  , updateCommand
  , historyCommand
  , cvsCatCommand
  , cvsMergeRevs
  , cvsDeleteCommand
  , cvsRemoveCommand
  , cvsShowAllHistory
  ) where

import FileSystemMod

import Data.Time.Clock
import System.Directory
import System.FilePath.Posix
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Data.List(deleteBy, intercalate)
import Data.Maybe

-- cd <folder> --

newCurrentPath :: [FilePath] -> MyFileSystem -> MyFileSystem
newCurrentPath paths fs = fs {fsGetCurrentPath = paths}

cd :: Monad m => FilePath -> StateT MyFileSystem (ExceptT String m) ()
cd path = do
  fs <- get
  let curPath = fsGetCurrentPath fs
  if path == ".."
  then
    case curPath of
      [] -> lift $ throwE "You are in init dir"
      _  -> modify $ newCurrentPath $ init curPath
  else do
    let paths = splitDirectories path
    if checkPath (getCurrentDirectory' fs) paths
    then modify $ newCurrentPath $ curPath ++ paths
    else lift $ throwE "No such directory"

-- dir --

dirCommand :: Monad m => StateT MyFileSystem (ExceptT String m) String
dirCommand = showDirContent . getCurrentDirectory' <$> get

-- ls <folder> --

changeInitDir :: Directory -> MyFileSystem -> MyFileSystem
changeInitDir dir fs = fs {fsGetInitDir = dir}

lsCommand :: Monad m => FilePath
                     -> StateT MyFileSystem (ExceptT String m) String
lsCommand path = do
  fs <- get
  let curDir = getCurrentDirectory' fs
  let paths  = splitDirectories path
  case foldM getDirInDir curDir paths of
    Just dir -> return $ showDirContent dir
    Nothing  -> lift $ throwE "No such directory"

-- create-folder "folder-name" --

emptyFolderPermissions :: Permissions
emptyFolderPermissions = setEmptyPermissions False True True True

emptyFilePermissions :: Permissions
emptyFilePermissions = setEmptyPermissions False False True True

setEmptyPermissions :: Bool -> Bool -> Bool -> Bool -> Permissions
setEmptyPermissions searchP execP writeP readP =
  setOwnerSearchable searchP $
  setOwnerExecutable execP $
  setOwnerWritable writeP $
  setOwnerReadable readP emptyPermissions

createFolder :: Monad m => String
                        -> StateT MyFileSystem (ExceptT String m) ()
createFolder name = do
  fs <- get
  if takeFileName name /= name
  then lift $ throwE "It is not name of folder"
  else do
    let curPath = getCurrentPath fs
    let folderPath = curPath </> name
    let folder = Directory { dGetDirectories = []
                           , dGetCvsDirs     = []
                           , dGetFiles       = []
                           , dGetPath        = folderPath
                           , dGetPermissions = emptyFolderPermissions
                           }
    let initDir = fsGetInitDir fs
    let paths = fsGetCurrentPath fs
    let runVal = changeDirectory initDir paths (addDirectory folder)
    case runExcept runVal of
      Right dir -> modify $ changeInitDir dir
      Left  str -> lift $ throwE str

addDirectory :: Directory -> Directory -> Except String Directory
addDirectory addDir dir =
  let dirs = dGetDirectories dir
  in if getDirectoryName addDir `elem` map getDirectoryName dirs
     then throwE "Directory already exists"
     else return $ dir {dGetDirectories = addDir : dirs}

-- create-file "file-name" --

createFile :: Monad m => String
                      -> UTCTime
                      -> StateT MyFileSystem (ExceptT String m) ()
createFile name time = do
  fs <- get
  if takeFileName name /= name
  then lift $ throwE "It is not name of file"
  else do
    let curPath = getCurrentPath fs
    let filePath = curPath </> name
    let file = File { fGetPath        = filePath
                    , fGetPermissions = emptyFilePermissions
                    , fGetExtension   = takeExtension name
                    , fGetTime        = time
                    , fGetSize        = 0
                    , fGetContent     = ""
                    }
    let initDir = fsGetInitDir fs
    let paths = fsGetCurrentPath fs
    let runVal = changeDirectory initDir paths (addFile file)
    case runExcept runVal of
      Right dir -> modify $ changeInitDir dir
      Left  str -> lift $ throwE str

addFile :: File -> Directory -> Except String Directory
addFile file dir =
  let files = dGetFiles dir
  in if getFileName file `elem` map getFileName files
     then throwE "File already exists"
     else return $ dir {dGetFiles = file : files}

-- cat <file> --

catCommand :: Monad m => FilePath
                      -> StateT MyFileSystem (ExceptT String m) String
catCommand path = do
  fs <- get
  let initDir = fsGetInitDir fs
  let (partPathes, name) = getPaths path
  let paths = fsGetCurrentPath fs ++ partPathes
  case getFile initDir paths name of
    Nothing -> lift $ throwE "File not found"
    Just f  -> return $ fGetContent f

-- write-file <file> "text" -- записать текст в файл

writeFileCommand :: Monad m => FilePath
                            -> String
                            -> UTCTime
                            -> StateT MyFileSystem (ExceptT String m) ()
writeFileCommand path text time = do
  fs <- get
  let initDir = fsGetInitDir fs
  let (partPathes, name) = getPaths path
  let paths = fsGetCurrentPath fs ++ partPathes
  case getFile initDir paths name of
    Nothing -> lift $ throwE "File not found"
    Just f  -> do
      let newFile = changeFileContent f text time
      let runVal  = changeDirectory initDir paths (changeFile newFile)
      case runExcept runVal of
        Right dir -> modify $ changeInitDir dir
        Left  str -> lift $ throwE str         

changeFile :: File -> Directory -> Except String Directory
changeFile file dir =
  let files = dGetFiles dir
      name  = getFileName file
  in case filter ((== name) . getFileName) files of
      [f] -> do
        let files' = deleteFile f files
        return $ dir {dGetFiles = file : files'}
      _ -> throwE "File not found"
  where
    eqFiles x y = getFileName x == getFileName y
    deleteFile = deleteBy eqFiles

changeFileContent :: File -> String -> UTCTime -> File
changeFileContent file content time = 
  let newSize = toInteger $ length content
  in file { fGetContent = content
          , fGetSize = newSize
          , fGetTime = time
          }

-- information <file> -- показать информацию о файле --
-- information <folder> -- показать информацию о директории --

infoFD :: Monad m => FilePath -> StateT MyFileSystem (ExceptT String m) String
infoFD path = do
  fs <- get
  let curDir = getCurrentDirectory' fs 
  case getFileOrDirectory curDir path of
    Nothing -> lift $ throwE "File or directory not found"
    Just val  ->
      case val of
        Left f  -> return $ showFileInfo f
        Right d -> return $ showDirectoryInfo d

-- find-file "file-name" --  поиск файла в текущией директории и поддиректориях

findFileCommand :: Monad m => String 
                           -> StateT MyFileSystem (ExceptT String m) String
findFileCommand name = do
  fs <- get
  let curDir = getCurrentDirectory' fs
  case findFileInDir name curDir of
    Just f -> return $ fGetPath f
    Nothing -> lift $ throwE "File not found"

findFileInDir :: String -> Directory -> Maybe File
findFileInDir name d@(Directory {dGetDirectories = dirs}) =
  case getFileInDir d name of
    Just f -> return f
    Nothing -> 
      case filter isJust (map (findFileInDir name) dirs) of
        (x : _) -> x
        [] -> Nothing

-- remove <folder | file> -- удалить выбранную директорию или файл

remCommand :: Monad m => FilePath -> StateT MyFileSystem (ExceptT String m) ()
remCommand path = do
  fs <- get
  let (partPathes, _) = getPaths path
  let initDir = fsGetInitDir fs
  let curDir = getCurrentDirectory' fs
  let paths = (fsGetCurrentPath fs) ++ partPathes
  case getFileOrDirectory curDir path of
    Nothing  -> lift $ throwE "File or directory not found"
    Just val ->
      case val of
        Left f  -> do
          let runVal  = changeDirectory initDir paths (delFile f)
          let res = runExcept runVal
          case res of
            Right dir -> modify $ changeInitDir dir
            Left  str -> lift $ throwE str  
        Right d -> do
          let runVal  = changeDirectory initDir paths (deleteDir d)
          case runExcept runVal of
            Right dir -> modify $ changeInitDir dir
            Left  str -> lift $ throwE str

delFile :: File -> Directory -> Except String Directory
delFile file dir =
  let files = dGetFiles dir
      name  = getFileName file
  in if name `notElem` map getFileName files
     then throwE "File not found"
     else return $ dir {dGetFiles = filter ((/= name) . getFileName) files}

deleteDir :: Directory -> Directory -> Except String Directory
deleteDir delDir dir =
  let dirs = dGetDirectories dir
      name = getDirectoryName delDir
  in if name `notElem` map getDirectoryName dirs
     then throwE "Directory not found"
     else return $ dir {dGetDirectories =
                          filter ((/= name) . getDirectoryName) dirs}

-- help --  показать руководство по использованию

helpCommand :: String
helpCommand =
  "cd <folder> -- перейти в директорию\n" ++
  "dir -- показать содержимое текущей директории\n" ++
  "ls <folder> -- показать содержимое выбранной директории\n" ++
  "create-folder \"folder-name\" -- создать директорию в текущей\n" ++
  "cat <file> -- показать содержимое файла\n" ++
  "create-file \"file-name\" -- создать пустой файл в текущей директории\n" ++
  "remove <folder | file> -- удалить выборанную директорию или файл\n" ++
  "write-file <file> \"text\" -- записать текст в файл" ++
  "find-file \"file-name\" " ++
  "--  поиск файла в текущией директории и поддиректориях\n" ++
  "information <file> -- показать информацию о файле\n" ++
  "information <folder> -- показать информацию о директории" ++
  "cvs-init -- инициализация СКВ в текущей выбранной директории\n" ++
  "cvs-add <file | folder> -- добавление файла или папки в СКВ\n" ++
  "cvs-update <file> \"comment\" -- добавление изменений файла в СКВ\n" ++
  "cvs-history <file> -- просмотр истории изменений файла\n" ++
  "cvs-cat <file> \"index\" -- просмотр конкретной ревизии файла\n" ++
  "cvs-merge-revs <file> \"index1\" \"index2\" \"left | right | both\" --\n" ++
  "объединение ревизий файла по заданным индексам, " ++
  "left, right, both или interactive\n" ++
  "являются вариантами стратегий для обеъединения\n" ++
  "cvs-delete-version <file> \"index\" " ++
  "-- удалить заданную версию файла из ревизий\n" ++
  "cvs-remove <file> -- удалить файл из СКВ\n" ++
  "cvs-show-everything -- показать общую историю изменений\n" ++
  "help --  показать руководство по использованию\n" ++
  "exit -- завершение работы программы\n"

-- newCvsDirectoryForFile --

newCvsDirectoryForFile :: UTCTime -> File -> Directory
newCvsDirectoryForFile time file =
  let (filePath, name) = splitFileName $ fGetPath file
      dirName = getCvsDirName name
      newCvsDirPath = filePath </> dirName
      initDir = commitDirectory time "0" "initial" newCvsDirPath file
  in Directory { dGetDirectories = [initDir]
               , dGetFiles       = []
               , dGetCvsDirs     = []
               , dGetPath        = newCvsDirPath
               , dGetPermissions = emptyFolderPermissions
               }

commitDirectory :: UTCTime -- createTimeForCommit
                -> String -- commitNumber
                -> String -- commit text
                -> FilePath -- parrentPath
                -> File -- fileForCommit
                -> Directory
commitDirectory time num commit path file = do
  let dirPath = path </> num
  let comDirName = takeFileName path
  let file' = fileToCommitDir file (comDirName </> num)
  let comFile = commitFile time commit dirPath
  Directory { dGetDirectories = []
            , dGetFiles       = [comFile, file']
            , dGetCvsDirs     = []
            , dGetPath        = dirPath
            , dGetPermissions = emptyFolderPermissions
            }

fileToCommitDir :: File -> String -> File
fileToCommitDir f@(File {fGetPath = path}) name =
  let prefix = dropFileName path
      suffix = takeFileName path
  in f {fGetPath = prefix </> name </> suffix}

commitFile :: UTCTime -> String -> FilePath -> File
commitFile time commit path =
  File { fGetPath        = path </> "commit.txt"
       , fGetPermissions = emptyFilePermissions
       , fGetExtension   = ".txt"
       , fGetTime        = time
       , fGetSize        = toInteger $ length commit
       , fGetContent     = commit
       }

-- initDirectory --

initDirectory :: UTCTime -> Directory -> Directory
initDirectory time dir@(Directory {dGetCvsDirs = cvsDir}) = do
  let files = dGetFiles dir
  let dirs = dGetDirectories dir
  let newCvsDirs = map (newCvsDirectoryForFile time) files
  let newDirs = map (initDirectory time) dirs
  let cvsNames = map getDirectoryName cvsDir
  let needNewPart = filter (\x -> getDirectoryName x `notElem` cvsNames) 
                           newCvsDirs
  dir { dGetDirectories = newDirs, dGetCvsDirs = cvsDir ++ needNewPart}

initDirectory' :: UTCTime -> Directory -> Except String Directory
initDirectory' time dir = return $ initDirectory time dir

-- initFile --

initFile :: UTCTime -> File -> Directory -> Except String Directory
initFile time file dir@(Directory {dGetCvsDirs = cvsDir}) = do
  let cvsNames = map getDirectoryName cvsDir
  if (getCvsDirName $ getFileName file) `elem` cvsNames
  then return dir
  else do
    let newCvsDir = newCvsDirectoryForFile time file
    return $ dir { dGetCvsDirs = newCvsDir : cvsDir}

-- cvs-init -- инициализация СКВ в текущей выбранной директории

initCommand :: Monad m => UTCTime
                       -> StateT MyFileSystem (ExceptT String m) ()
initCommand time = addCvsCommand time ""

-- cvs-add <file | folder> -- добавление файла или папки в СКВ

addCvsCommand :: Monad m => UTCTime
                         -> FilePath
                         -> StateT MyFileSystem (ExceptT String m) ()
addCvsCommand time path = do
  fs <- get
  let (partPathes, _) = getPaths path
  let initDir = fsGetInitDir fs
  let curDir = getCurrentDirectory' fs
  let paths = (fsGetCurrentPath fs) ++ partPathes
  case getFileOrDirectory curDir path of
    Nothing  -> lift $ throwE "File or directory not found"
    Just val ->
      case val of
        Left f  -> do
          let runVal  = changeDirectory initDir paths (initFile time f)
          let res = runExcept runVal
          case res of
            Right dir -> modify $ changeInitDir dir
            Left  str -> lift $ throwE str  
        Right _ -> do
          let paths' = splitDirectories path
          let runVal  = changeDirectory initDir paths' (initDirectory' time)
          case runExcept runVal of
            Right dir -> modify $ changeInitDir dir
            Left  str -> lift $ throwE str

-- cvs-update <file> "comment" -- добавление изменений файла в СКВ

updateCommand :: Monad m => UTCTime
                         -> FilePath
                         -> String
                         -> StateT MyFileSystem (ExceptT String m) ()
updateCommand time path commit =
  runCvsCommandDependsOfFile path (updateFile time commit)

updateFile :: UTCTime -> String -> File -> Directory -> Except String Directory
updateFile time commit file dir =
  let cvs = dGetCvsDirs dir
      name  = getFileName file
  in case filter ((== getCvsDirName name) . getDirectoryName) cvs of
      [d] -> do
        let newCommitDir = commitDirectory time
                                           (getCommitNumber d)
                                           commit
                                           (dGetPath d)
                                           file
        let dirsInD = dGetDirectories d 
        let d' = d {dGetDirectories = dirsInD ++ [newCommitDir]}
        replaceCvsDir d' dir
      _ -> throwE "Untracked file"

getCommitNumber :: Directory -> String
getCommitNumber Directory {dGetDirectories = dirs} =
  let m = maximum $ map ((read :: String -> Integer) . getDirectoryName) dirs
  in show $ m + 1

replaceCvsDir :: Directory -- cvsDir
              -> Directory -- parentDir
              -> Except String Directory -- newParentDir
replaceCvsDir cvsDir dir@(Directory {dGetCvsDirs = cvs}) = do
  let name = getDirectoryName cvsDir
  if name `elem` map getDirectoryName cvs
  then do
    let newCvs = filter ((/= name) . getDirectoryName) cvs
    return dir {dGetCvsDirs = newCvs ++ [cvsDir]}
  else throwE "Replaced directory not found"

-- cvs-history <file> -- просмотр истории изменений файла

showSmthForFile :: Monad m => FilePath
                           -> (Directory -> Except String String)
                           -> StateT MyFileSystem (ExceptT String m) String
showSmthForFile path func = do
  fs <- get
  let (partPathes, name) = getPaths path
  let dirName = getCvsDirName name
  let initDir = fsGetInitDir fs
  let paths = fsGetCurrentPath fs ++ partPathes
  case getDirectory initDir paths of
    Nothing -> lift $ throwE "Incorrect path"
    Just dir ->
      case getFileInDir dir name of
        Nothing -> lift $ throwE "File not found"
        Just _ -> do
          let cvsDir = getCvsDirInDir dir dirName
          case cvsDir of
            Nothing -> lift $ throwE "Untracked file"
            Just d ->
              case runExcept (func d) of
                Left err -> lift $ throwE err
                Right str -> return $ str

historyCommand :: Monad m => FilePath
                          -> StateT MyFileSystem (ExceptT String m) String
historyCommand path = showSmthForFile path showHistory

showHistory :: Directory -> Except String String
showHistory Directory {dGetDirectories = dirs} =
  return $ intercalate "\n" (map showCommit dirs)

-- cvs-cat <file> "index" -- просмотр конкретной ревизии файла

cvsCatCommand :: Monad m => FilePath
                         -> String
                         -> StateT MyFileSystem (ExceptT String m) String
cvsCatCommand path index = showSmthForFile path $
                                           showVersion (takeFileName path) 
                                                        index

showVersion :: String -> String -> Directory -> Except String String
showVersion name index dir = 
  case getDirInDir dir index of
    Nothing -> throwE $ "Version " ++ index ++ " not found"
    Just d ->
      case getFileInDir d name of
        Just file -> return $ fGetContent file
        Nothing -> throwE "Not found file in cvs!!!"

-- cvs-merge-revs <file> "index1" "index2" "left | right | both " --

cvsMergeRevs :: Monad m => UTCTime  -- current time
                        -> FilePath -- path to file
                        -> String   -- index1
                        -> String   -- index2
                        -> String   -- strategy
                        -> StateT MyFileSystem (ExceptT String m) ()
cvsMergeRevs time path index1 index2 strategy =
  case strategy of
    "left" -> do
      fileText1 <- cvsCatCommand path index1
      writeFileCommand path fileText1 time
      updateCommand time path "merge-left"
    "right" -> do
      fileText2 <- cvsCatCommand path index2
      writeFileCommand path fileText2 time
      updateCommand time path "merge-right"
    "both" -> do
      fileText1 <- cvsCatCommand path index1
      fileText2 <- cvsCatCommand path index2
      let text = fileText1 ++ "\n>>>>\n" ++ fileText2
      writeFileCommand path text time
      updateCommand time path "merge-both"
    _ -> lift $ throwE "Unknown strategy"

-- cvs-delete-version <file> "index" --удалить заданную версию файла из ревизий

cvsDeleteCommand :: Monad m => FilePath -- path to file
                            -> String   -- version index
                            -> StateT MyFileSystem (ExceptT String m) ()
cvsDeleteCommand path index =
  runCvsCommandDependsOfFile path $ deleteVersion index

deleteVersion :: String   -- index
              -> File
              -> Directory
              -> Except String Directory
deleteVersion index file dir =
  let cvs = dGetCvsDirs dir
      name  = getFileName file
  in case filter ((== getCvsDirName name) . getDirectoryName) cvs of
      [d] -> do
        let dirs = dGetDirectories d
        if notElem index $ map getDirectoryName dirs
        then throwE $ "Version " ++ index ++ " not found"
        else do
          let newDirs = filter ((/= index) . getDirectoryName) dirs
          let d' = d {dGetDirectories = newDirs}
          replaceCvsDir d' dir
      _ -> throwE "Untracked file"

runCvsCommandDependsOfFile :: Monad m => FilePath
                                      -> (File
                                      -> Directory
                                      -> Except String Directory)
                                      -> StateT MyFileSystem (ExceptT String m) ()
runCvsCommandDependsOfFile path changeCommand = do
  fs <- get
  let initDir = fsGetInitDir fs
  let (partPathes, name) = getPaths path
  let paths = fsGetCurrentPath fs ++ partPathes
  case getFile initDir paths name of
    Nothing -> lift $ throwE "File not found"
    Just f  -> do
      let runVal  = changeDirectory initDir paths (changeCommand f)
      case runExcept runVal of
        Right dir -> modify $ changeInitDir dir
        Left  str -> lift $ throwE str

-- cvs-remove <file> -- удалить файл из СКВ

cvsRemoveCommand :: Monad m => FilePath
                            -> StateT MyFileSystem (ExceptT String m) ()
cvsRemoveCommand path = do
  fs <- get
  let (partPathes, name) = getPaths path
  let initDir = fsGetInitDir fs
  let curDir = getCurrentDirectory' fs
  let paths = fsGetCurrentPath fs ++ partPathes
  let parrentDir = getDirectory curDir partPathes
  case parrentDir of
    Nothing -> lift $ throwE "Incorrect path"
    Just pd ->
      case getFileInDir pd name of
        Nothing -> lift $ throwE "File not found"
        Just _  ->
          case getCvsDirInDir pd (getCvsDirName name) of
            Nothing -> lift $ throwE "Untracked file"
            Just d -> do
              let runVal  = changeDirectory initDir paths (deleteCvsDir d)
              case runExcept runVal of
                Right dir -> modify $ changeInitDir dir
                Left  str -> lift $ throwE str

deleteCvsDir :: Directory -> Directory -> Except String Directory
deleteCvsDir delDir dir =
  let dirs = dGetCvsDirs dir
      name = getDirectoryName delDir
  in if notElem name $ map getDirectoryName dirs
     then throwE "Directory not found"
     else return $ dir {dGetCvsDirs = filter ((/= name) . getDirectoryName) dirs}

-- cvs-show-everything
cvsShowAllHistory :: Monad m => StateT MyFileSystem (ExceptT String m) String
cvsShowAllHistory = do
  fs <- get
  let initDir = fsGetInitDir fs
  return $ showDirectoryHistory initDir

showDirectoryHistory :: Directory -> String
showDirectoryHistory dir = do
  let dirs = dGetCvsDirs dir
  let inDirs = dGetDirectories dir
  let dirPaths = map dGetPath dirs
  let showHistory' = map showHistory dirs
  let showHistory'' = sequence showHistory'
  case runExcept showHistory'' of
    Left _ -> error "ERROR showDirectoryHistory"
    Right hs -> do
      let names = map takeFileName dirPaths
      let fileNames = map (drop (length getPrefix)) names
      let paths = map dropFileName dirPaths
      let filePathes = map (uncurry (</>)) (zip paths fileNames)
      let pairInfo = zip filePathes hs
      let info = map (\(x, y) -> x ++ ":\n" ++ y) pairInfo
      let infoText = intercalate "\n" info
      let dirsText = intercalate "\n" (map showDirectoryHistory inDirs)
      infoText ++ "\n" ++ dirsText
