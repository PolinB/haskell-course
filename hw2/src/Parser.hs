{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser(..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , spaceParser
  ) where

import Control.Applicative(Alternative(..))
import Data.Char(isSpace)

-- | Parser for list of elements.
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

-- | Apply a function to the first element of a pair.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- | Implement Functor instance for Parser.
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser $ fmap (first f) . p

-- | Implement Applicative instance for Parser.
instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser (\ss -> Just (x, ss))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (Parser ff) <*> (Parser fx) = Parser func
    where
      func ss = do
                (fn, s1) <- ff ss
                (a, s2) <- fx s1
                return (fn a, s2)

-- | Implement Monad instance for Parser.
instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser x >>= f = Parser func
    where
      func ss = do
                (a, s1) <- x ss
                runParser (f a) s1

-- | Implement Alternative instance for Parser.
instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  x <|> y = Parser func
    where
      func ss = do
                let a = runParser x ss
                let b = runParser y ss
                a <|> b

-- | The parser never crashes or absorbs input.
ok :: Parser s ()
ok = Parser (\s -> return ((), s))

-- | Check that the parser has reached the end
-- of the data stream (otherwise it crashes).
eof :: Parser s ()
eof = Parser func
  where
    func [] = Just ((), [])
    func _ = Nothing

-- The parser takes a predicate on a stream element,
-- and returns an element that absorbs it from the
-- stream, if the predicate on the element is True,
-- otherwise it falls.
satisfy :: (s -> Bool) -> Parser s s
satisfy pr = Parser func
  where
    func [] = Nothing
    func (x : xs) =
      if pr x
      then Just (x, xs)
      else Nothing

-- | Parse one stream element.
element :: Eq s => s -> Parser s s
element el = satisfy (== el)

-- | Parse several stream elements.
stream :: Eq s => [s] -> Parser s [s]
stream [] = Parser (\s -> Just ([], s))
stream (x : xs) = Parser func
  where
    func ss = do
              (y, ys) <- runParser (element x) ss
              (z, zs) <- runParser (stream xs) ys
              return (y : z, zs)

-- | A speces parser.
spaceParser :: Parser Char ()
spaceParser = (satisfy isSpace *> spaceParser) <|> ok
