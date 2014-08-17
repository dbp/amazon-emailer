{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad (forM, replicateM_)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Control.Exception.Lifted as EX
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime, getCurrentTime)

import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import Data.Configurator (load, Worth(..), require, lookupDefault)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.HTTP.Conduit (Manager(..), newManager, closeManager, conduitManagerSettings)
import Network.Mail.Mime (Mail(..), Address(..), Part(..), Encoding (QuotedPrintableText))
import Network.Mail.Mime.SES

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
  accessKey <- require config "ses-access-key"
  secretKey <- require config "ses-secret-key"
  limit <- require config "limit"
  bracket ((,) <$> (connect $ defaultConnectInfo { connectHost = host,
                                                   connectPort = port,
                                                   connectUser = user,
                                                   connectPassword = pass,
                                                   connectDatabase = db })
               <*> (newManager conduitManagerSettings))
    (\(c,m) -> close c >> closeManager m)
    (\(c,m) -> runQueue limit accessKey secretKey c m)

data AmEmail = AmEmail { aId :: Int, aTo :: Text, aToName :: Maybe Text, aFrom :: Text,
                         aFromName :: Text, aSubject :: Text, aBody :: Text, aHtml :: Bool }
             deriving (Show, Eq)

instance FromRow AmEmail where
  fromRow = AmEmail <$> field <*> field <*> field <*> field
                    <*> field <*> field <*> field <*> field


runQueue :: Int -> ByteString -> ByteString -> Connection -> Manager -> IO ()
runQueue limit access secret c m = do
  now <- getCurrentTime
  replicateM_ limit $ do
    -- NOTE(dbp 2013-12-12): This query has all the magic in it: it grabs an email
    -- off the queue, marks it as processing, and returns it. The nested query is to
    -- do the limiting, and the seeming redundant 'and processing = false' is b/c
    -- the inner query is in a separate transaction, so this could race (but the
    -- result would just be to have no message found even if one existed in database.
    email <- query_ c "update amazon_email_queue set processing = true where id = (select id from amazon_email_queue where sent_at is null and failed_count < 3 and processing = false order by date asc limit 1) and processing = false returning id, to_addr, to_name, from_addr, from_name, subject, body, html"
    sent <- EX.catch
            (sendEmails access secret m email)
            (\e -> do print (e::EX.SomeException)
                      execute c "update amazon_email_queue set failed_count = failed_count + 1, processing = false where id = ?"
                        (Only (aId $ head email))
                      return [])
    mapM (\i -> execute c "update amazon_email_queue set sent_at = ?, processing = false where id = ?"
                      (now, i))
            sent

  threadDelay 1000000 -- note, this is a conservative processing of the queue, as we don't include
                      -- the time that is spent actually sending the emails.
  runQueue limit access secret c m

sendEmails :: ByteString -> ByteString -> Manager ->  [AmEmail] -> IO [Int]
sendEmails access secret m es =
  forM es (\e -> do
            runResourceT (renderSendMailSES m (mkSES access secret e) (mkMail e))
            return (aId e))

mkSES :: ByteString -> ByteString -> AmEmail -> SES
mkSES access secret (AmEmail _ to _ from _ _ _ _) = SES (T.encodeUtf8 from)
                                                      [T.encodeUtf8 to]
                                                      access
                                                      secret

mkMail :: AmEmail -> Mail
mkMail (AmEmail _ to tname from fname subj body html) =
  Mail (Address (Just fname) from)
       [Address tname to]
       [] -- CC
       [] -- BCC
        [ ("Subject", subj) ]
       [[Part (if html then "text/html; charset=utf-8" else "text/plain; charset=utf-8")
         QuotedPrintableText Nothing []
         (LT.encodeUtf8 $ LT.fromStrict body)]]
