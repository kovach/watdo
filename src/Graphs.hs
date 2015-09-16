module Graphs where

import Data.List (sortOn)

import Log

burn' :: [Name] -> Log Entry -> [(Time, Weight)]
burn' names log =
  let es = map (flip lookt log) names
      es' = sortOn fst es
      step (time, entry) p@(total, chain) =
        case entry of
          Task _ ->
            let nw = total + 1 in
            (nw, (time, nw) : chain)
          Arrow from ty to ->
            case ty of
              -- done tag subtracts from total
              Tag -> case look from log of
                Label "done" ->
                  -- TODO get weight of `to`
                  let nw = total - 1 in
                  (nw, (time, nw) : chain)
                Label _ -> p
                _ -> error "?"
              _ -> p -- ??
          -- New task adds to total
          _ -> p
  in snd $ foldr step (0, []) es'
  
--burn :: [(Name, Time, Weight, Type)] -> Log Entry -> [(Time, Weight)]
--burn events log =
--  let e = sortOn (\(a, _, _, _) -> a) events
--      add (from, time, weight, tag) p@(total, chain) =
--        case tag of
--          Tag -> case look from log of
--            Label "done" ->
--              let nw = total - weight in
--              (nw, (time, nw) : chain)
--            Label _ -> p
--            _ -> error "?"
--          Subtask ->
--            let nw = weight + total in
--            (nw, (time, nw) : chain)
--          Comment -> p
--  in snd $ foldr add (0, []) e
