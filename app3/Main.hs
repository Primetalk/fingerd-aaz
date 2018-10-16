{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.IORef 
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
-- import Data.ByteString.Lazy

import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)

import Data.Aeson
import Data.Aeson.Text

import User
import Db

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =   
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8090 (spock spockCfg app)

extractUserFromBody :: MonadIO m => ActionT m (Maybe User)
extractUserFromBody = decodeStrict <$> body

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get "/users" $ do
         liftIO $ putStrLn "getUsers"
         conn <- liftIO $ open "finger.db"
         users <- liftIO $ getUsers conn
         liftIO $ SQLite.close conn
         let serialized = encode users
         bytes $ BS.toStrict serialized <> "\n"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
       post ("/adduser/" <//> var) $ \name ->
         do
           maybeUser <- extractUserFromBody
           case maybeUser of
             Nothing -> do
               text $ "No user data found in body for" <> name <> "\n"
               return ()
             Just user -> do
               liftIO $ withDb (addUser user)
               text $ "Success: " <> name <> "\n"
               return ()
