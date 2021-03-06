module  ParserTest
        ( testCd
        , testDir
        , testLs
        , testCreateFolder
        , testCat
        , testCreateFile
        , testRemove
        , testWriteFile
        , testFindFile
        , testInformation
        , testCvsInit
        , testCvsAdd
        , testCvsUpdate
        , testCvsHistory
        , testCvsCat
        , testCvsMergeRevs
        , testCvsDeleteVersion
        , testCvsRemove
        , testCvsShowEverything
        , testHelp
        , testExit
        ) where

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

import CommandParser
import Parser

testCd :: Spec
testCd = describe "test cd parser" $ do
  it "correct input: cd lla/asas/df_re.re" $
    let input = "cd lla/asas/df_re.re"
    in runParser commandParser input
       `shouldBe`
       Just ((Cd "lla/asas/df_re.re"), "")
  it "incorrect input: cd' dsad" $
    let input = "cd' dsad"
    in runParser commandParser input
       `shouldBe`
       Nothing

testDir :: Spec
testDir = describe "test dir parser" $ do
  it "correct input: dir" $
    let input = "dir"
    in runParser commandParser input
       `shouldBe`
       Just (Dir, "")
  it "incorrect input: dir path" $
    let input = "diraa"
    in runParser commandParser input
       `shouldBe`
       Nothing

testLs :: Spec
testLs = describe "test ls parser" $
  it "correct input: ls a/a/a.txt" $
    let input = "ls a/a/a.txt"
    in runParser commandParser input
       `shouldBe`
       Just (Ls "a/a/a.txt", "")

testCreateFolder :: Spec
testCreateFolder = describe "test create-folder parser" $
  it "correct input: create-folder a" $
    let input = "create-folder \"a\""
    in runParser commandParser input
       `shouldBe`
       Just (CreateFolder "a", "")

testCat :: Spec
testCat = describe "test cat parser" $
  it "correct input: cat a/a/d.txt" $
    let input = "cat a/a/d.txt"
    in runParser commandParser input
       `shouldBe`
       Just (Cat "a/a/d.txt", "")

testCreateFile :: Spec
testCreateFile = describe "test create-file parser" $
  it "correct input: create-file \"d.txt\"" $
    let input = "create-file \"d.txt\""
    in runParser commandParser input
       `shouldBe`
       Just (CreateFile "d.txt", "")

testRemove :: Spec
testRemove = describe "test remove parser" $ do
  it "correct input: remove d.txt" $
    let input = "remove d.txt"
    in runParser commandParser input
       `shouldBe`
       Just (Remove "d.txt", "")
  it "correct input: remove d/d.txt" $
    let input = "remove d/d.txt"
    in runParser commandParser input
       `shouldBe`
       Just (Remove "d/d.txt", "")

testWriteFile :: Spec
testWriteFile = describe "test write-file command" $
  it "correct input: write-file d \"hello))\"" $
    let input = "write-file d \"hello))\""
    in runParser commandParser input
       `shouldBe`
       Just (WriteFile "d" "hello))", "")

testFindFile :: Spec
testFindFile = describe "test find-file command" $
  it "correct input: find-file \"hello\"" $
    let input = "find-file \"hello\""
    in runParser commandParser input
       `shouldBe`
       Just (FindFile "hello", "")

testInformation :: Spec
testInformation = describe "test information command" $
  it "correct input: information file" $
    let input = "information file"
    in runParser commandParser input
       `shouldBe`
       Just (Information "file", "")

testCvsInit :: Spec
testCvsInit = describe "test cvs-init command" $
  it "correct input: cvs-init" $
    let input = "cvs-init"
    in runParser commandParser input
       `shouldBe`
       Just (CvsInit, "")

testCvsAdd :: Spec
testCvsAdd = describe "test cvs-add command" $
  it "correct input: cvs-add file" $
    let input = "cvs-add file"
    in runParser commandParser input
       `shouldBe`
       Just (CvsAdd "file", "")

testCvsUpdate :: Spec
testCvsUpdate = describe "test cvs-update command" $
  it "correct input: cvs-update file \"comment\"" $
    let input = "cvs-update file \"comment\""
    in runParser commandParser input
       `shouldBe`
       Just (CvsUpdate "file" "comment", "")

testCvsHistory :: Spec
testCvsHistory = describe "test cvs-history command" $
  it "correct input: cvs-history file" $
    let input = "cvs-history file"
    in runParser commandParser input
       `shouldBe`
       Just (CvsHistory "file", "")

testCvsCat :: Spec
testCvsCat = describe "test cvs-cat command" $
  it "correct input: cvs-cat file \"index\"" $
    let input = "cvs-cat file \"index\""
    in runParser commandParser input
       `shouldBe`
       Just (CvsCat "file" "index", "")

testCvsMergeRevs :: Spec
testCvsMergeRevs = describe "test cvs-merge-revs command" $ do
  it "correct input: cvs-merge-revs file \"index1\" \"index2\" \"left\"" $
    let input = "cvs-merge-revs file \"index1\" \"index2\" \"left\""
    in runParser commandParser input
       `shouldBe`
       Just (CvsMergeRevs "file" "index1" "index2" "left", "")
  it "correct input: cvs-merge-revs file \"index1\" \"index2\" \"right\"" $
    let input = "cvs-merge-revs file \"index1\" \"index2\" \"right\""
    in runParser commandParser input
       `shouldBe`
       Just (CvsMergeRevs "file" "index1" "index2" "right", "")
  it "correct input: cvs-merge-revs file \"index1\" \"index2\" \"both\"" $
    let input = "cvs-merge-revs file \"index1\" \"index2\" \"both\""
    in runParser commandParser input
       `shouldBe`
       Just (CvsMergeRevs "file" "index1" "index2" "both", "")
  it "incorrect input: cvs-merge-revs file \"index1\" \"index2\" \"other\"" $
    let input = "cvs-merge-revs file \"index1\" \"index2\" \"other\""
    in runParser commandParser input
       `shouldBe`
       Nothing

testCvsDeleteVersion :: Spec
testCvsDeleteVersion = describe "test cvs-delete-version command" $
  it "correct input: cvs-delete-version file \"index\"" $
    let input = "cvs-delete-version file \"index\""
    in runParser commandParser input
       `shouldBe`
       Just (CvsDeleteVersion "file" "index", "")

testCvsRemove :: Spec
testCvsRemove = describe "test cvs-remove command" $
  it "correct input: cvs-remove file " $
    let input = "cvs-remove file "
    in runParser commandParser input
       `shouldBe`
       Just (CvsRemove "file", "")

testCvsShowEverything :: Spec
testCvsShowEverything = describe "test cvs-cvs-show-everything command" $
  it "correct input: cvs-show-everything" $
    let input = "   cvs-show-everything    "
    in runParser commandParser input
       `shouldBe`
       Just (AllHistory, "")

testHelp :: Spec
testHelp = describe "test help command" $
  it "correct input: help" $
  let input = "help"
  in runParser commandParser input
       `shouldBe`
       Just (Help, "")

testExit :: Spec
testExit = describe "test exit command" $
  it "correct input: exit" $
  let input = "exit"
  in runParser commandParser input
       `shouldBe`
       Just (Exit, "")
