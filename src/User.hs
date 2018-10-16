{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
module User where

import GHC.Generics
import Control.Exception
import Data.Text (Text)
import Data.Aeson

import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import qualified Database.SQLite.Simple.Types as SqlTypes
import Text.RawString.QQ

data User =
  User {
      userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
  } deriving (Eq, Show, Generic)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir
    realName phone) =
    toRow (id_, username, shell, homeDir,
      realName, phone)

instance ToJSON User where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User
    -- No need to provide a parseJSON implementation.
         
createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser =
  "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers =
  "SELECT * FROM users"

getUserQuery :: Query
getUserQuery =
  "SELECT * FROM users WHERE username = ?"


data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser row1
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where row1 :: UserRow
        row1 = (SqlTypes.Null, "primetalk", "/bin/sh",
                 "/home/primetalk", "Primetalk", "123-345-5678")

getUsers :: Connection -> IO [User]
getUsers conn = do
  results <- query conn allUsers ()
  return results

addUser :: User -> Connection -> IO ()
addUser user connection =
  execute connection insertUser user
