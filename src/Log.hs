{-# LANGUAGE OverloadedStrings #-}
module Log where

import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Control.Monad

type Map = M.Map

type Name = Int
type Timestamp = Int
type Log a = (Int, Int, Map Name (Timestamp, a))

type From = Name
type To = Name

-- Arrow types
-- TODO allow arbitrary Entry to label an Arrow?
data Type = Comment | Subtask | Tag
  deriving (Show, Eq, Ord)

data Entry
  = Blob Text
  -- Blob
  | Task Name
  | Arrow From Type To
  | Label String
  deriving (Show, Eq, Ord)

task (Task t) = t
label (Label s) = s

add :: a -> Log a -> IO (Log a)
add x (a, r, m) = do
  ts <- fmap round getPOSIXTime
  return (a+1, r, M.insert (a+1) (ts, x) m)
adds :: [a] -> Log a -> IO (Log a)
adds xs m = foldM (flip add) m xs

look :: Name -> Log a -> a
look n (_, _, e) = snd . fromJust $ M.lookup n e

name :: Log a -> Name
name (n, _, _) = n

root :: Log a -> Name
root (_, r, _) = r

setRoot r (a, _, b) = (a, r, b)

env :: Log a -> Map Name (Timestamp, a)
env (_, _, e) = e

comment :: Text -> Name -> Log Entry -> IO (Log Entry)
comment str name m = do
  m@(text, _, _) <- add (Blob str) m
  m <- add (Arrow text Comment name) m
  return m

initialWorld :: IO (Log Entry)
initialWorld = do
  let m = (-1, -1, M.empty)
  m <- add (Blob "root") m
  m@(root, _, _) <- add (Task (name m)) m
  m <- add (Label "in-progress") m
  m <- add (Label "done") m
  m <- add (Label "deleted") m
  return $ setRoot root m
  
children :: Name -> Log Entry -> [(Name, Type)]
children name (_, _, m) = map snd $ M.toList $ M.mapMaybe (isArrowTo name) m
  where
    isArrowTo target (_, Arrow from ty to) | target == to = Just (from, ty)
    isArrowTo _ _ = Nothing

factor :: Ord a => [(a, b)] -> Map a [b]
factor = factor' M.empty
  where
    factor' m [] = m
    factor' m ((a, b) : rest) = factor' (M.insertWith (++) a [b] m) rest

swap (a, b) = (b, a)

splitChildren :: Name -> Log Entry -> Map Type [Name]
splitChildren name log = factor . map swap $ children name log
