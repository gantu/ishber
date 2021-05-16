module Model where

import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.Aeson
import GHC.Generics
import Servant.Auth.JWT
import ClassyPrelude
import Validation
import Text.Regex.PCRE.Heavy

data EmailValidationErr = EmailValidationErrInvalidEmail

data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber

newtype Email = Email {emailRaw :: Text } deriving (Show, Eq, FromJSON)
newtype Password = Password {passwordRaw :: Text } deriving (Show, Eq, FromJSON)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches 
    [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
    "Not a Valid Email"
  ]

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
      [ lengthBetween 5 50 "Should between 5 and 50"
      , regexMatches [re|\d|] "Should contain number"
      , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
      , regexMatches [re|[a-z]|] "Should contain lowercase letter"
      ]

rawPassword :: Password -> Text
rawPassword = passwordRaw

share 
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User 
    name Text 
    email Text 
    password Text 
    age Int 
    occupation Text
    UniqueEmail email
    deriving Show Read Generic
  |]


instance ToJSON User 
instance ToJWT User

instance FromJSON User
instance FromJWT User

data Login = Login 
  { email :: Email
  , password :: Password 
  } deriving (Eq, Show,Generic)

instance ToJSON Login where
  toJSON (Login email password) = object ["email" .= rawEmail email, "password" .= rawPassword password]

instance FromJSON Login where
  parseJSON (Object v) = do 
    email <- v .: "email"
    password <- v .: "password"
    return (Login {email = email, password = password})
  parseJSON _ = ClassyPrelude.empty 

doMigration :: SqlPersistT IO()
doMigration = runMigration migrateAll

