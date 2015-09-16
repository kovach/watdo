{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8 (putStrLn)
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Control.Monad

-- TODO remove
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import Log
import Conversion
import Server


main = do
  m <- initialWorld
  m <- add (Blob "T1") m
  m@(task, _, _) <- add (Task (name m)) m
  m <- add (Blob "Ta") m
  m@(ta, _, _) <- add (Task (name m)) m
  m <- add (Blob "Tb") m
  m@(tb, _, _) <- add (Task (name m)) m
  m <- add (Arrow ta Subtask task) m
  m <- add (Arrow tb Subtask task) m
  m@(tb, _, _) <- add (Task (name m)) m
  m@(title, _, _) <- add (Blob "comment on task") m
  m <- add (Arrow title Comment task) m
  m@(scott, _, _) <- add (Label "scott") m
  m <- add (Arrow scott Tag task) m
  m <- add (Arrow task Subtask (root m)) m
  m <- add (Arrow scott Tag (root m)) m
  runServer m
  --t <- getLine
  --m <- comment (T.pack t) task m
  --print m
