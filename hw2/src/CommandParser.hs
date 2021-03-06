module CommandParser
  ( commandParser
  , Command(..)
  ) where

import Parser

import Control.Applicative(Alternative(..))
import Data.Char(isAlphaNum)

-- | Data for all commands. Contains all user input information.
-- <..> - relative path to file.
data Command = Cd String               -- ^ cd <folder>
             | Dir                     -- ^ dir
             | Ls String               -- ^ ls <folder>
             | CreateFolder String     -- ^ create-folder "folder-name"
             | Cat String              -- ^ cat <file>
             | CreateFile String       -- ^ create-file "file-name"
             | Remove String           -- ^ remove <file/folder>
             | WriteFile String String -- ^ write-file <file> "text"
             | FindFile String         -- ^ find-file "file-name"
             | Information String      -- ^ information <file/folder>
             | Exit                    -- ^ exit
             | Help                    -- ^ help
             | CvsInit                 -- ^ cvs-init
             | CvsAdd String           -- ^ cvs-add <file/folder>
             | CvsUpdate String String -- ^ cvs-update <file> "commit"
             | CvsHistory String       -- ^ cvs-history <file>
             | CvsCat String String    -- ^ cvs-cat <file> "index"
             | CvsMergeRevs String     -- ^ cvs-merge-revs <file>
                            String     -- ^ "index1" - first version
                            String     -- ^ "index2" - second version
                            String     -- ^ "left/right/both" - strategy
             | CvsDeleteVersion String -- ^ cvs-delete <file>
                                String -- ^ "index"
             | CvsRemove String        -- ^ cvs-remove <file>
             | AllHistory              -- ^ cvs-show-everything
  deriving(Show, Eq)

isPartOfPath :: Char -> Bool
isPartOfPath ch = isAlphaNum ch || (ch == '/') || (ch == '.') || (ch == '_')

pathParser :: Parser Char String
pathParser = (\x -> (++) [x]) <$> satisfy isPartOfPath
             <*> pathParser'
  where
    pathParser' = (\x -> (++) [x]) <$> satisfy isPartOfPath
                  <*> pathParser' <|> "" <$ ok

textParser :: Parser Char String
textParser = element '"' *> ((\x -> (++) [x]) <$>
    satisfy (/= '"')) <*> textParser' <* element '"'
  where
    textParser' = (\x -> (++) [x]) <$>
                  satisfy (/= '"') <*> textParser' <|> "" <$ ok

commandConstrParser :: String
                    -> (String -> Command)
                    -> Parser Char String
                    -> Parser Char Command
commandConstrParser name constr parser =
  stream name *> spaceParser *> (constr <$> parser) <* spaceParser <* eof

commandWithTextParser :: String
                      -> (String -> String -> Command)
                      -> Parser Char Command
commandWithTextParser name constr =
  stream name *> spaceParser 
              *> (constr <$> (pathParser <* spaceParser) <*> textParser)
              <* spaceParser <* eof

cdParser :: Parser Char Command
cdParser = commandConstrParser "cd" Cd pathParser

dirParser :: Parser Char Command
dirParser = (Dir <$ stream "dir") <* spaceParser <* eof

lsParser :: Parser Char Command
lsParser = commandConstrParser "ls" Ls pathParser

createFoldParser :: Parser Char Command
createFoldParser = commandConstrParser "create-folder" CreateFolder textParser

catParser :: Parser Char Command
catParser = commandConstrParser "cat" Cat pathParser

createFileParser :: Parser Char Command
createFileParser = commandConstrParser "create-file" CreateFile textParser

removeParser :: Parser Char Command
removeParser = commandConstrParser "remove" Remove pathParser

writeFileParser :: Parser Char Command
writeFileParser = commandWithTextParser "write-file" WriteFile

findFileParser :: Parser Char Command
findFileParser = commandConstrParser "find-file" FindFile textParser

infoParser :: Parser Char Command
infoParser = commandConstrParser "information" Information pathParser

exitParser :: Parser Char Command
exitParser = spaceParser *> (Exit <$ stream "exit") <* spaceParser <* eof

helpParser :: Parser Char Command
helpParser = spaceParser *> (Help <$ stream "help") <* spaceParser <* eof

initParser :: Parser Char Command
initParser = spaceParser *> (CvsInit <$ stream "cvs-init") 
                         <* spaceParser 
                         <* eof

cvsAddParser :: Parser Char Command
cvsAddParser = commandConstrParser "cvs-add" CvsAdd pathParser

cvsUpdateParser :: Parser Char Command
cvsUpdateParser = commandWithTextParser "cvs-update" CvsUpdate

cvsHistoryParser :: Parser Char Command
cvsHistoryParser = commandConstrParser "cvs-history" CvsHistory pathParser

cvsCatParser :: Parser Char Command
cvsCatParser = commandWithTextParser "cvs-cat" CvsCat

cvsMergeRevsParser :: Parser Char Command
cvsMergeRevsParser =
  stream "cvs-merge-revs" 
  *> spaceParser
  *> (CvsMergeRevs <$> (pathParser <* spaceParser) 
                   <*> (textParser <* spaceParser)
                   <*> (textParser <* spaceParser)
                   <*> (element '"'
                    *> (stream "left" 
                   <|> stream "right"
                   <|> stream "both")
                   <*  element '"'))
                   <* spaceParser <* eof

cvsDeleteVersionParser :: Parser Char Command
cvsDeleteVersionParser = commandWithTextParser "cvs-delete-version" CvsDeleteVersion

cvsRemoveParser :: Parser Char Command
cvsRemoveParser = commandConstrParser "cvs-remove" CvsRemove pathParser

allHistoryParser :: Parser Char Command
allHistoryParser = spaceParser
                *> (AllHistory <$ stream "cvs-show-everything")
                <* spaceParser <* eof

-- | Parser for input command. Return Just Command if
-- has correct input, else return Nothing.
commandParser :: Parser Char Command
commandParser = cdParser
            <|> dirParser
            <|> lsParser
            <|> createFoldParser
            <|> catParser
            <|> createFileParser
            <|> removeParser
            <|> writeFileParser
            <|> findFileParser
            <|> infoParser
            <|> exitParser
            <|> helpParser
            <|> initParser
            <|> cvsAddParser
            <|> cvsUpdateParser
            <|> cvsHistoryParser
            <|> cvsCatParser
            <|> cvsMergeRevsParser
            <|> cvsDeleteVersionParser
            <|> cvsRemoveParser
            <|> allHistoryParser
