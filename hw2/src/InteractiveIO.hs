module InteractiveIO
  ( interactive
  , handler
  , handlerWithRes
  ) where

import FileSystemMod
import Commands
import Parser
import CommandParser

import Data.Time.Clock
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import System.IO

-- | Run interactive for programm. Read commands from IO,
-- do it with our file system and show results or errors for user.
interactive :: MyFileSystem -> IO MyFileSystem
interactive fs = do
  let curPath = getCurrentPath fs
  putStr $ curPath ++ " > "
  hFlush stdout
  command <- getLine
  curTime <- getCurrentTime
  case runParser commandParser command of
    Nothing  -> do
      putStrLn "Unknown command"
      hFlush stdout
      interactive fs
    Just res -> do
      let com = fst res
      case com of
        Exit -> return fs
        Cd s -> runCommand fs $ cd s
        Dir  -> runCommandWithRes fs dirCommand
        Ls s -> runCommandWithRes fs $ lsCommand s
        CreateFolder s -> runCommand fs $ createFolder s
        Cat s -> runCommandWithRes fs $ catCommand s
        CreateFile s -> runCommand fs $ createFile s curTime
        Remove s -> runCommand fs $ remCommand s
        WriteFile s text -> runCommand fs $ writeFileCommand s text curTime
        FindFile s -> runCommandWithRes fs $ findFileCommand s
        Information s -> runCommandWithRes fs $ infoFD s
        Help -> do
          putStr helpCommand
          interactive fs
        CvsInit -> runCommand fs $ initCommand curTime
        CvsAdd s -> runCommand fs $ addCvsCommand curTime s
        CvsUpdate s c -> runCommand fs $ updateCommand curTime s c
        CvsHistory s -> runCommandWithRes fs $ historyCommand s
        CvsCat s v -> runCommandWithRes fs $ cvsCatCommand s v
        CvsMergeRevs s i1 i2 str -> runCommand fs $
                                      cvsMergeRevs curTime s i1 i2 str
        CvsDeleteVersion s v -> runCommand fs $ cvsDeleteCommand s v
        CvsRemove s -> runCommand fs $ cvsRemoveCommand s
        AllHistory -> runCommandWithRes fs cvsShowAllHistory

runCommand :: MyFileSystem 
           -> StateT MyFileSystem (ExceptT String IO) ()
           -> IO MyFileSystem
runCommand fs st = do
  newFs <- handler fs st
  interactive newFs

runCommandWithRes :: MyFileSystem 
                  -> StateT MyFileSystem (ExceptT String IO) String
                  -> IO MyFileSystem
runCommandWithRes fs st = do
  newFs <- handlerWithRes fs st
  interactive newFs

-- | Run command without result on file system and return results file system.
-- Also show errors, if they occurred.
handler :: MyFileSystem
        -> StateT MyFileSystem (ExceptT String IO) ()
        -> IO MyFileSystem
handler fs st = do
  runSt <- runExceptT $ runStateT st fs
  case runSt of
    Left err -> do putStrLn err
                   hFlush stdout
                   return fs
    Right newFs -> return $ snd newFs

-- | Run command with result on file system and return results file system.
-- Also show command results and errors, if they occurred.
handlerWithRes :: MyFileSystem
               -> StateT MyFileSystem (ExceptT String IO) String
               -> IO MyFileSystem
handlerWithRes fs st = do
  runSt <- runExceptT $ runStateT st fs
  case runSt of
    Left err    -> do putStrLn err
                      hFlush stdout
                      return fs
    Right newFs -> do putStrLn $ fst newFs
                      hFlush stdout
                      return $ snd newFs
