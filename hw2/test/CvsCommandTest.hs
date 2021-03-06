module CvsCommandTest
  ( testInitCommand
  , testAddCommand
  , testUpdateCommand
  , testHistoryCommand
  , testCatCommand
  , testMergeCommand
  , testDeleteVersionCommand
  , testRemoveCommand
  , testShowEverythingCommand
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
import Data.List(intercalate)

testDir :: IO FilePath
testDir = do
  curDir <- getCurrentDirectory
  return $ curDir </> "testDir"

getCvsDirInfo :: Directory -> String
getCvsDirInfo dir =
  intercalate "\n" (map showCommitDir (dGetDirectories dir))

showCommitDir :: Directory -> String
showCommitDir dir =
  "Num: " ++ getDirectoryName dir ++ " Files: " ++ unwords (map showFile (dGetFiles dir))

showFile :: File -> String
showFile file =
  "(" ++ fGetPath file ++ ", " ++ fGetContent file ++ ")"

initCommitC :: String -> String
initCommitC path =
  "Num: 0 " ++
  "Files: (" ++ path </> ".cvs_c.txt/0/commit.txt" ++ ", initial) " ++ 
  "(" ++ path </> ".cvs_c.txt/0/c.txt" ++ ", cccccc)"

addCommitC :: String -> String
addCommitC path = initCommitC path ++ "\n" ++
  "Num: 1 " ++ 
  "Files: (" ++ path </> ".cvs_c.txt/1/commit.txt" ++ ", first) " ++
  "(" ++ path </> ".cvs_c.txt/1/c.txt" ++ ", test)"

leftMergeC :: String -> String
leftMergeC path = addCommitC path ++ "\n" ++
  "Num: 2 " ++
  "Files: (" ++ path </> ".cvs_c.txt/2/commit.txt" ++ ", merge-left) " ++
  "(" ++ path </> ".cvs_c.txt/2/c.txt" ++ ", cccccc)"

rightMergeC :: String -> String
rightMergeC path = addCommitC path ++ "\n" ++
  "Num: 2 " ++
  "Files: (" ++ path </> ".cvs_c.txt/2/commit.txt" ++ ", merge-right) " ++
   "(" ++ path </> ".cvs_c.txt/2/c.txt" ++ ", test)"

bothMergeC :: String -> String
bothMergeC path = addCommitC path ++ "\n" ++
  "Num: 2 " ++
  "Files: (" ++ path </> ".cvs_c.txt/2/commit.txt" ++ ", merge-both) " ++
   "(" ++ path </> ".cvs_c.txt/2/c.txt" ++ ", cccccc\n>>>>\ntest)"

testInitCommand :: Spec
testInitCommand = describe "test init command" $ do
  it "init -- check start dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    let startDir = fsGetInitDir fs1
    let cvsDirs = dGetCvsDirs startDir
    let [x] = filter ((== ".cvs_c.txt") . getDirectoryName) cvsDirs
    getCvsDirInfo x `shouldBe` initCommitC testPath
  it "init -- check dira dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    fs2 <- handler fs1 $ cd "dira"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` [".cvs_a.txt"]
  it "init -- check dira/diraa dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    fs2 <- handler fs1 $ cd "dira/diraa"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` [".cvs_lala.txt"]
  it "init -- check dirb dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    fs2 <- handler fs1 $ cd "dirb"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` [".cvs_c.txt"]

testAddCommand :: Spec
testAddCommand = describe "test add command" $ do
  it "add 'dira' -- check start dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ addCvsCommand curTime "dira"
    let startDir = fsGetInitDir fs1
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` []
  it "add 'dira' -- check dira dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ addCvsCommand curTime "dira"
    fs2 <- handler fs1 $ cd "dira"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` [".cvs_a.txt"]
  it "add 'dira' -- check dira/diraa dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ addCvsCommand curTime "dira"
    fs2 <- handler fs1 $ cd "dira/diraa"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` [".cvs_lala.txt"]
  it "add 'dira' -- check dirb dir" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ addCvsCommand curTime "dira"
    fs2 <- handler fs1 $ cd "dirb"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` []

testUpdateCommand :: Spec
testUpdateCommand = describe "test update command" $ do
  it "update 'c.txt' 'first'" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    let [x] = filter ((== ".cvs_c.txt") . getDirectoryName) cvsDirs
    getCvsDirInfo x `shouldBe` addCommitC testPath
  it "cat 'c.txt' '2' -- untracked file" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ addCvsCommand curTime "dira"
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    runSt <- runExceptT $ runStateT (updateCommand curTime "c.txt" "first") fs1
    runSt `shouldBe` Left "Untracked file"

testHistoryCommand :: Spec
testHistoryCommand = describe "test history command" $ do
  it "history 'c.txt' -- init history" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    Right newFs <- runExceptT $ runStateT (historyCommand "c.txt") fs'
    fst newFs `shouldBe` "0. initial"
  it "history 'c.txt' -- after update" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    Right newFs <- runExceptT $ runStateT (historyCommand "c.txt") fs2
    fst newFs `shouldBe` "0. initial\n1. first"
  it "history 'c.txt' -- untracked file" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ addCvsCommand curTime "dira"
    runSt <- runExceptT $ runStateT (historyCommand "c.txt") fs'
    runSt `shouldBe` Left "Untracked file"

testCatCommand :: Spec
testCatCommand = describe "test cat command" $ do
  it "cat 'c.txt' '0'" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    Right newFs <- runExceptT $ runStateT (cvsCatCommand "c.txt" "0") fs2
    fst newFs `shouldBe` "cccccc"
  it "cat 'c.txt' '1'" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    Right newFs <- runExceptT $ runStateT (cvsCatCommand "c.txt" "1") fs2
    fst newFs `shouldBe` "test"
  it "cat 'c.txt' '2' -- not found version" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    runSt <- runExceptT $ runStateT (cvsCatCommand "c.txt" "2") fs2
    runSt `shouldBe` (Left "Version 2 not found")
  it "cat 'c.txt' '0' -- untracked file" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ addCvsCommand curTime "dira"
    runSt <- runExceptT $ runStateT (cvsCatCommand "c.txt" "0") fs'
    runSt `shouldBe` Left "Untracked file"

testMergeCommand :: Spec
testMergeCommand = describe "test merge command" $ do
  it "merge 'c.txt' '0' '1' 'left' -- check commit directory" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsMergeRevs curTime "c.txt" "0" "1" "left"
    let startDir = getCurrentDirectory' fs3
    let cvsDirs = dGetCvsDirs startDir
    let [x] = filter ((== ".cvs_c.txt") . getDirectoryName) cvsDirs
    getCvsDirInfo x `shouldBe` leftMergeC testPath
  it "merge 'c.txt' '0' '1' 'left' -- check file" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsMergeRevs curTime "c.txt" "0" "1" "left"
    Right st <- runExceptT $ runStateT (catCommand "c.txt") fs3
    fst st `shouldBe` "cccccc"
  it "merge 'c.txt' '0' '1' 'right' -- check commit directory" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsMergeRevs curTime "c.txt" "0" "1" "right"
    let startDir = getCurrentDirectory' fs3
    let cvsDirs = dGetCvsDirs startDir
    let [x] = filter ((== ".cvs_c.txt") . getDirectoryName) cvsDirs
    getCvsDirInfo x `shouldBe` rightMergeC testPath
  it "merge 'c.txt' '0' '1' 'right' -- check file" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsMergeRevs curTime "c.txt" "0" "1" "right"
    Right st <- runExceptT $ runStateT (catCommand "c.txt") fs3
    fst st `shouldBe` "test"
  it "merge 'c.txt' '0' '1' 'both' -- check commit directory" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsMergeRevs curTime "c.txt" "0" "1" "both"
    let startDir = getCurrentDirectory' fs3
    let cvsDirs = dGetCvsDirs startDir
    let [x] = filter ((== ".cvs_c.txt") . getDirectoryName) cvsDirs
    getCvsDirInfo x `shouldBe` bothMergeC testPath
  it "merge 'c.txt' '0' '1' 'both' -- check file" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsMergeRevs curTime "c.txt" "0" "1" "both"
    Right st <- runExceptT $ runStateT (catCommand "c.txt") fs3
    fst st `shouldBe` "cccccc\n>>>>\ntest"
  it "merge 'c.txt' '0' '2' 'both' -- version not found" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    st <- runExceptT $ runStateT (cvsMergeRevs curTime "c.txt" "0" "2" "both") fs2
    st `shouldBe` Left "Version 2 not found"

testDeleteVersionCommand :: Spec
testDeleteVersionCommand = describe "test merge command" $ do
  it "delete-version 'c.txt' '1'" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    fs3 <- handler fs2 $ cvsDeleteCommand "c.txt" "1"
    let startDir = getCurrentDirectory' fs3
    let cvsDirs = dGetCvsDirs startDir
    let [x] = filter ((== ".cvs_c.txt") . getDirectoryName) cvsDirs
    getCvsDirInfo x `shouldBe` initCommitC testPath
  it "delete-version 'c.txt' '0' -- untracked file" $ do
    testPath <- testDir
    fs <- getFileSystemIO testPath
    st <- runExceptT $ runStateT (cvsDeleteCommand "c.txt" "0") fs
    st `shouldBe` Left "Untracked file"
  it "delete-version 'c.txt' '1' -- version not found" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    st <- runExceptT $ runStateT (cvsDeleteCommand "c.txt" "1") fs1
    st `shouldBe` Left "Version 1 not found"

testRemoveCommand :: Spec
testRemoveCommand = describe "test remove command" $ do
  it "remove 'c.txt'" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    fs2 <- handler fs1 $ cvsRemoveCommand "c.txt"
    let startDir = getCurrentDirectory' fs2
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` []
  it "remove 'dira/a.txt'" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs1 <- handler fs $ initCommand curTime
    fs2 <- handler fs1 $ cvsRemoveCommand "dira/a.txt"
    fs3 <- handler fs2 $ cd "dira"
    let startDir = getCurrentDirectory' fs3
    let cvsDirs = dGetCvsDirs startDir
    map getDirectoryName cvsDirs `shouldBe` []

testShowEverythingCommand :: Spec
testShowEverythingCommand = describe "test show-everything command" $
  it "show-everything" $ do
    testPath <- testDir
    curTime <- getCurrentTime
    fs <- getFileSystemIO testPath
    fs' <- handler fs $ initCommand curTime
    fs1 <- handler fs' $ writeFileCommand "c.txt" "test" curTime
    fs2 <- handler fs1 $ updateCommand curTime "c.txt" "first"
    let pathC = testPath </> "c.txt"
    let pathA = testPath </> "dira/a.txt"
    let pathLala = testPath </> "dira/diraa/lala.txt"
    let pathDC = testPath </> "dirb/c.txt"
    let res = pathC ++ ":\n" ++
              "0. initial\n1. first\n" ++
              pathA ++ ":\n" ++
              "0. initial\n" ++
              pathLala ++ ":\n" ++
              "0. initial\n\n" ++
              pathDC ++ ":\n" ++
              "0. initial\n"
    Right newFs <- runExceptT $ runStateT cvsShowAllHistory fs2
    fst newFs `shouldBe` res
