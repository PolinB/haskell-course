module Main
      ( main
      ) where

import Test.Tasty.Hspec (hspec)

import ParserTest
import FsCommandTest
import CvsCommandTest

main :: IO ()
main = hspec $ do
  ParserTest.testCd
  ParserTest.testDir
  ParserTest.testLs
  ParserTest.testCreateFolder
  ParserTest.testCat
  ParserTest.testCreateFile
  ParserTest.testRemove
  ParserTest.testWriteFile
  ParserTest.testFindFile
  ParserTest.testInformation
  ParserTest.testCvsInit
  ParserTest.testCvsAdd
  ParserTest.testCvsUpdate
  ParserTest.testCvsHistory
  ParserTest.testCvsCat
  ParserTest.testCvsMergeRevs
  ParserTest.testCvsDeleteVersion
  ParserTest.testCvsRemove
  ParserTest.testCvsShowEverything
  ParserTest.testHelp
  ParserTest.testExit

  FsCommandTest.testCdCommand
  FsCommandTest.testDirCommand
  FsCommandTest.testLsCommand
  FsCommandTest.testCreateFolderCommand
  FsCommandTest.testCatCommand
  FsCommandTest.testCreateFileCommand
  FsCommandTest.testRemoveCommand
  FsCommandTest.testWriteFileCommand
  FsCommandTest.testFindFileCommand
  FsCommandTest.testInformation

  CvsCommandTest.testInitCommand
  CvsCommandTest.testAddCommand
  CvsCommandTest.testUpdateCommand
  CvsCommandTest.testHistoryCommand
  CvsCommandTest.testCatCommand
  CvsCommandTest.testMergeCommand
  CvsCommandTest.testDeleteVersionCommand
  CvsCommandTest.testRemoveCommand
  CvsCommandTest.testShowEverythingCommand
