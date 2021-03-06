module  FsCommandTest
        ( testCdCommand
        , testDirCommand
        , testLsCommand
        , testCreateFolderCommand
        , testCatCommand
        , testCreateFileCommand
        , testRemoveCommand
        , testWriteFileCommand
        , testFindFileCommand
        , testInformation
        ) where

import FileSystemMod
import BuildStructure
import Commands
import InteractiveIO

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix

testDir :: IO FilePath
testDir = do
  curDir <- getCurrentDirectory
  return $ curDir </> "testDir"

testCdCommand :: Spec
testCdCommand = describe "test cd command" $ do
  it "cd 'dira' -- check FS current array" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    fsGetCurrentPath fs1 `shouldBe` ["dira"]

  it "cd 'dira' -- check FS getCurrentPath" $ do
    testPath <- testDir 
    let dirPath = testPath </> "dira"
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    getCurrentPath fs1 `shouldBe` dirPath

  it "cd 'dira' -- check FS getCurrentDirectory'" $ do
    testPath <- testDir 
    let dirPath = testPath </> "dira"
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    let curDir = getCurrentDirectory' fs1 
    dGetPath curDir `shouldBe` dirPath

  it "cd 'dira/diraa' -- check FS current array" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira/diraa"
    (fsGetCurrentPath fs1) `shouldBe` ["dira", "diraa"]

  it "cd 'dira/diraa' -- check FS getCurrentPath" $ do
    testPath <- testDir 
    let dirPath = testPath </> "dira/diraa"
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira/diraa"
    getCurrentPath fs1 `shouldBe` dirPath

  it "cd 'dira/diraa' -- check FS getCurrentDirectory'" $ do
    testPath <- testDir 
    let dirPath = testPath </> "dira/diraa"
    fs  <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira/diraa"
    let curDir = getCurrentDirectory' fs1 
    dGetPath curDir `shouldBe` dirPath

  it "cd 'dirc' -- check no such directory" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (cd "dirc") fs
    runSt `shouldBe` Left "No such directory"

  it "cd '..' -- check FS current array" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    fs2 <- handler fs1 $ cd ".."
    fsGetCurrentPath fs2 `shouldBe` []

  it "cd '..' -- check FS getCurrentPath" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    fs2 <- handler fs1 $ cd ".."
    getCurrentPath fs2 `shouldBe` testPath

  it "cd '..' -- check FS getCurrentDirectory'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    fs2 <- handler fs1 $ cd ".."
    let curDir = getCurrentDirectory' fs2 
    dGetPath curDir `shouldBe` testPath

testDirCommand :: Spec
testDirCommand = describe "test dir command" $ do
  it "dir -- in init dir" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT dirCommand fs
    fst newFs `shouldBe` "Directories:\ndira dirb\nFiles:\nc.txt"

  it "dir -- after cd dira" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ cd "dira"
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\ndiraa\nFiles:\na.txt"

testLsCommand :: Spec
testLsCommand = describe "test ls command" $ do
  it "ls 'dira'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (lsCommand "dira") fs
    fst newFs `shouldBe` "Directories:\ndiraa\nFiles:\na.txt"

  it "ls 'dira/diraa'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (lsCommand "dira/diraa") fs
    fst newFs `shouldBe` "Directories:\n\nFiles:\nlala.txt"

testCreateFolderCommand :: Spec
testCreateFolderCommand = describe "test create-folder command" $ do
  it "create-folder '\"folder\"'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ createFolder "folder"
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\nfolder dira dirb\nFiles:\nc.txt"

  it "create-folder '\"dira\"' -- incorrect" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (createFolder "dira") fs
    runSt `shouldBe` Left "Directory already exists"

  it "create-folder '\"a/folder\"' -- incorrect" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (createFolder "a/folder") fs
    runSt `shouldBe` Left "It is not name of folder"

testCatCommand :: Spec
testCatCommand = describe "test cat command" $ do
  it "cat 'c.txt'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (catCommand "c.txt") fs
    fst newFs `shouldBe` "cccccc"

  it "cat 'dira/diraa/lala.txt'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (catCommand "dira/diraa/lala.txt") fs
    fst newFs `shouldBe` "lala"

  it "cat 'dira/diraa/c.txt' -- incorrect" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (catCommand "dira/diraa/c.txt") fs
    runSt `shouldBe` Left "File not found"

testCreateFileCommand :: Spec
testCreateFileCommand = describe "test create-file command" $ do
  it "create-file '\"file\"'" $ do
    curTime <- getCurrentTime
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ createFile "file" curTime
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\ndira dirb\nFiles:\nfile c.txt"

  it "create-file '\"c.txt\"' -- incorrect" $ do
    curTime <- getCurrentTime
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (createFile "c.txt" curTime) fs
    runSt `shouldBe` Left "File already exists"

  it "create-file '\"a/file.txt\"' -- incorrect" $ do
    curTime <- getCurrentTime
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (createFile "a/file.txt" curTime) fs
    runSt `shouldBe` Left "It is not name of file"

testRemoveCommand :: Spec
testRemoveCommand = describe "test remove command" $ do
  it "remove 'c.txt' -- remove file" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "c.txt"
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\ndira dirb\nFiles:\n"

  it "remove 'a.txt' -- incorrect" $ do
    testPath <- testDir
    fs  <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (remCommand "a.txt") fs
    runSt `shouldBe` Left "File or directory not found"

  it "remove 'dira' -- remove directory" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "dira"
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\ndirb\nFiles:\nc.txt"

  it "remove 'dira/a.txt' -- check init dir" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "dira/a.txt"
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\ndira dirb\nFiles:\nc.txt"

  it "remove 'dira/a.txt' -- check dira dir" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "dira/a.txt"
    Right newFs <- runExceptT $ runStateT (lsCommand "dira") fs1
    fst newFs `shouldBe` "Directories:\ndiraa\nFiles:\n"

  it "remove 'dira/diraa' -- check init dir" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "dira/diraa"
    Right newFs <- runExceptT $ runStateT dirCommand fs1
    fst newFs `shouldBe` "Directories:\ndira dirb\nFiles:\nc.txt"

  it "remove 'dira/diraa' -- check dira dir" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "dira/diraa"
    Right newFs <- runExceptT $ runStateT (lsCommand "dira") fs1
    fst newFs `shouldBe` "Directories:\n\nFiles:\na.txt"

testWriteFileCommand :: Spec
testWriteFileCommand = describe "test write-file command" $ do
  it "write-file 'c.txt' '\"new text\"'" $ do
    curTime <- getCurrentTime
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ writeFileCommand "c.txt" "new text" curTime
    Right newFs <- runExceptT $ runStateT (catCommand "c.txt") fs1
    fst newFs `shouldBe` "new text"
  it "write-file 'dira/diraa/lala.txt' '\"text\"'" $ do
    curTime <- getCurrentTime
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ writeFileCommand "dira/diraa/lala.txt" "text" curTime
    Right newFs <- runExceptT $ runStateT (catCommand "dira/diraa/lala.txt") fs1
    fst newFs `shouldBe` "text"
  it "write-file 'dira/diraa/c.txt' '\"text\"'-- incorrect" $ do
    curTime <- getCurrentTime
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT 
                            (writeFileCommand "dira/diraa/c.txt" "text" curTime)
                            fs
    runSt `shouldBe` Left "File not found"

testFindFileCommand :: Spec
testFindFileCommand = describe "test find-file command" $ do
  it "find-file 'c.txt'" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (findFileCommand "c.txt") fs
    fst newFs `shouldBe` testPath </> "c.txt"
  it "find-file 'c.txt' -- after delete in testDir" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ remCommand "c.txt"
    Right newFs <- runExceptT $ runStateT (findFileCommand "c.txt") fs1
    fst newFs `shouldBe` testPath </> "dirb/c.txt"
  it "find-file 'd.txt' -- not found" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (findFileCommand "d.txt") fs
    runSt `shouldBe` Left "File not found"
  it "find-file 'dira' -- not found, because it's directory" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    runSt <- runExceptT $ runStateT (findFileCommand "dira") fs
    runSt `shouldBe` Left "File not found"

testInformation :: Spec
testInformation = describe "test information command" $ do
  it "information 'c.txt' -- file info" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    setModificationTime (testPath </> "c.txt") curTime
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (infoFD "c.txt") fs
    let filePath = testPath </> "c.txt"
    let res = "File path: " ++ filePath ++ "\n" ++
              "Permissions: readable writable \n" ++
              "Type: .txt\n" ++ 
              "Last update time: " ++ show curTime ++ "\n" ++
              "Size: 6 bytes\n"
    fst newFs `shouldBe` res
  it "information 'dira' -- dir info" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    Right newFs <- runExceptT $ runStateT (infoFD "dira") fs
    let dirPath = testPath </> "dira"
    let res = "Directory path: " ++ dirPath ++ "\n" ++
              "Permissions: readable writable searchable \n" ++
              "Files in dir: 1\n" ++
              "Size: 9 bytes\n"
    fst newFs `shouldBe` res
  it "information 'd' -- file or directory not found" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    res <- runExceptT $ runStateT (infoFD "d") fs
    res `shouldBe` Left "File or directory not found"
