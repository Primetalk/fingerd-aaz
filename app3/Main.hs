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

import User
import Data.Aeson
import Data.Aeson.Text

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8090 (spock spockCfg app)

-- extractUserFromBody :: ActionCtxT ctx IO (Maybe User) -- //T T.Text IO Maybe User
-- extractUserFromBody = do
--   b <- body
--   let user = (decodeStrict b) :: Maybe User
--   return user

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get "/users" $ do
         liftIO $ putStrLn "getUsers"
         conn <- liftIO $ open "finger.db"
         users <- liftIO $ getUsers conn
         liftIO $ SQLite.close conn
         let serialized = encodeStrict users
         -- text $ T.pack $ BS.unpack
         bytes  serialized
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
       post ("/adduser/" <//> var) $ \name ->
         do
--           extractUserFromBody
--           b <- body
--           user <- liftIO $ (decodeStrict b) :: Maybe User
           text "Success"
