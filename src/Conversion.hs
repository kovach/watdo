{-# LANGUAGE OverloadedStrings #-}
module Conversion where

import qualified Data.Map as M
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Log

data Thing = Thing Name Text [String] [Thing]
  deriving (Show, Eq, Ord)

-- !! TODO !!
-- include name

t2v :: Thing -> Value
t2v (Thing name title tags children) = object [
  "name" .= T.pack (show name),
  "head" .= title,
  "tags"  .= map T.pack tags,
  "children" .= (map t2v children)]

t1 = Thing 0 "T1" ["scott", "in-progress"] [Thing 1 "Ta" [] [], Thing 2 "Tb" [] []]

mlook k = fromMaybe [] . M.lookup k

n2t :: Log Entry -> Name -> Thing
n2t log name =
  let leaves = splitChildren name log
      tags = map (label . flip look log) . fromMaybe [] $ M.lookup Tag leaves
      -- !! TODO
      comms = mlook Comment leaves
      -- !! TODO

      subs = mlook Subtask leaves

      Task n = look name log
      Blob descr = look n log

  in Thing name descr tags (map (n2t log) $ subs)

--saveThing :: Value -> Log Entry -> IO (Log Entry)
--saveThing val log =
--  withObject "huh" $ \obj -> do
    


--example:
--val1 :: Value
--val1 = object [
--  "title" .= ("T1":: Text),
--  "tags"  .= ["scott" :: Text, "in-progress"],
--  "children" .= [
--    object ["title" .= ("Ta" :: Text)],
--    object ["title" .= ("Tb" :: Text)]
--  ]
-- ]
