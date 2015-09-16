{-# LANGUAGE OverloadedStrings #-}
module Server where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B8 (toString)

import qualified Network.WebSockets as WS

import Log
import Conversion

type Id = Int
type Client = (Id, WS.Connection)
data ServerState = SS
  { count :: Int
  , clients :: [Client]
  , world :: Log Entry}

initialState :: Log Entry -> ServerState
initialState i = SS
  { count = 0
  , clients = []
  , world = i }

broadcast :: Id -> Text -> [Client] -> IO ()
broadcast id msg clients = forM_ clients send 
  where
    send (id', conn) | id /= id' = WS.sendTextData conn msg
    send _ = return ()

runServer :: Log Entry -> IO ()
runServer w = do
  state <- newMVar (initialState w)
  WS.runServer "0.0.0.0" 9160 $ application state

type App = MVar ServerState -> IO ()

send :: Id -> Text -> App
send id text state = do
  SS {clients = cs} <- readMVar state
  let Just conn = lookup id cs
  WS.sendTextData conn text

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  SS {world = w, count = count} <- readMVar state
  let id = count + 1
  putStrLn $ "User #" ++ show id ++ " connected?"
  modifyMVar_ state $ \s -> return s {count = id, clients = (id, conn) : clients s}
  send id (T.pack . B8.toString . encode . t2v . n2t w $ root w) state
  finally (talk id conn state) (disconnect id state)

removeClient id s = s { clients = filter (not . (== id) . fst) (clients s) }

prefixMsg id msg = T.concat [T.pack (show id), ": ", msg]

talk id conn state = forever $ do
  msg <- WS.receiveData conn
  SS {clients = cs} <- readMVar state
  broadcast id (prefixMsg id msg) cs

disconnect id state = do
  putStrLn $ "User #" ++ show id ++ " disconnected?"
  modifyMVar_ state $ \s -> return $ removeClient id s
  SS {clients = cs} <- readMVar state
  broadcast id (prefixMsg id "disconnected.") cs

