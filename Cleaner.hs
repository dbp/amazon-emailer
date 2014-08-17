{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)

import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import Data.Configurator (load, Worth(..), require, lookupDefault)

import Data.String (fromString)

import Database.PostgreSQL.Simple

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then putStrLn "config file argument required" >> exitWith (ExitFailure 1)
     else do
  config <- load [Required (head args)]
  host <- lookupDefault "127.0.0.1" config "host"
  port <- lookupDefault 5432 config "port"
  user <- require config "user"
  pass <- require config "pass"
  db <- require config "db"
  cols <- require config "columns"
  bracket (connect $ defaultConnectInfo { connectHost = host,
                                          connectPort = port,
                                          connectUser = user,
                                          connectPassword = pass,
                                          connectDatabase = db })
    close
    (runQueue cols)

runQueue :: String -> Connection -> IO ()
runQueue cols c = do
  withTransaction c $ do
    execute_ c (fromString $ "INSERT INTO amazon_email_archive SELECT " ++ cols ++ " FROM amazon_email_queue WHERE sent_at IS NOT NULL")
    execute_ c "DELETE FROM amazon_email_queue WHERE sent_at IS NOT NULL"

  threadDelay 60000000 -- clean once per minute
  runQueue cols c
